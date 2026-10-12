#!/usr/bin/env python3
"""index.qmd の「バージョン情報」の表を，pdfs/ に置いた PDF の版数から書き直す。

quarto render の pre-render で毎回走る。pdftotext が無いマシンでは何もしない
(表はそのまま残る)。版数は各 PDF の冒頭3ページにある「version X.Y.Z」を読む。
"""
import os
import re
import shutil
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INDEX = os.path.join(ROOT, "index.qmd")

# 表の行(表示名, PDF ファイル名)。並び順もこのとおり
ROWS = [
    ("基礎シラバス", "syllabus_basic.pdf"),
    ("基礎テキスト", "BasicBook3.pdf"),
    ("応用シラバス2A", "syllabus2a.pdf"),
    ("応用シラバス2B", "syllabus2b.pdf"),
    ("応用テキスト1(前期)", "Dkiso2_book1.pdf"),
    ("応用テキスト2(後期)", "Dkiso2_book2.pdf"),
]


def pdf_version(path):
    out = subprocess.run(["pdftotext", "-f", "1", "-l", "3", path, "-"],
                         capture_output=True, text=True).stdout
    m = re.search(r"version\s+(\d+\.\d+\.\d+)", out)
    return m.group(1) if m else None


def main():
    if shutil.which("pdftotext") is None:
        sys.stderr.write("update_versions: pdftotext が無いので表は更新しない\n")
        return 0
    lines = ["| 資料 | 最新バージョン |", "|------|--------------|"]
    for label, fname in ROWS:
        v = pdf_version(os.path.join(ROOT, "pdfs", fname))
        lines.append(f"| [{label}](pdfs/{fname}) | {v or '-'} |")
    table = "\n".join(lines)
    with open(INDEX, encoding="utf-8") as fh:
        text = fh.read()
    pat = re.compile(r"\| 資料 \| 最新バージョン \|\n\|[-|]+\|\n(?:\|.*\|\n?)+")
    if not pat.search(text):
        sys.stderr.write("update_versions: 表が見つからない\n")
        return 0
    new = pat.sub(table + "\n", text, count=1)
    if new != text:
        with open(INDEX, "w", encoding="utf-8") as fh:
            fh.write(new)
    return 0


if __name__ == "__main__":
    sys.exit(main())
