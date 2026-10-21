# kosugi-stats プロジェクト

## 概要
- **公開URL**: https://stats.kosugitti.net/ (GitHub Pages既定URL: https://kosugitti.github.io/kosugi-stats/)
- **ローカル**: `~/Dropbox/Git/kosugi-stats/`
- **形式**: Quarto Website (GitHub Pages, docs/デプロイ)
- **用途**: 心理統計教育教材ポータル（psychometrics_syllabus の後継）
- **リポジトリ**: kosugitti/kosugi-stats (public)

## 状態
- 公開済み、稼働中
- stats.kosugitti.net はHTTPS対応済み（Let's Encrypt証明書発行・`https_enforced=true`）
- 基礎ページの「各章のコード」(`codes/Dkiso1/`)はテキストのコードと一致させる。テキストのコードを直したら，ここのファイルも直す。`codes/` の .qmd は render 対象外(`_quarto.yml`)

### GitHub Pagesの証明書が効かなくなったら
「安全なサイトではない」警告が出る場合，`gh api repos/kosugitti/kosugi-stats/pages` で
`https_certificate.state` を確認する。`NONE` ならカスタムドメインを解除→再設定で発行が走る（詳細な手順と経緯はWORKLOG.md 2026-08-01）。

## 構成
```
kosugi-stats/
├── _quarto.yml
├── index.qmd          # トップ（設計方針・バージョン情報）
├── basic.qmd          # データ解析基礎
├── applied.qmd        # データ解析応用
├── lectures.qmd       # 特別講義
├── codes.qmd          # コード・データ一覧
├── about.qmd          # ライセンス・連絡先
├── pdfs/              # 現行PDF（10ファイル）
├── archive/           # 旧版PDF（2ファイル）
├── codes/             # Rコード・サンプルデータ
│   ├── SampleData/    # CSV 11ファイル
│   ├── Dkiso1/        # 基礎用Rコード(章ごとの完成版.R・ワークシート.qmd・課題データ。旧版ファイルも同居)
│   └── Dkiso2/        # 応用用Rコード 28ファイル
└── docs/              # Quarto出力
```

## LaTeXソースとの関係
- LaTeXソース（テキスト原稿）は `~/Dropbox/Git/psychometrics_syllabus/Psychometrics/` にある
- ここにはコンパイル済みPDFとWebサイトのみ置く

---

## ユーザ操作サポート

### 「テキストを更新した」「PDFを差し替えたい」と言われたら

1. 更新されたPDFがどれか確認（psychometrics_syllabus でコンパイル済みのはず）
2. PDFをコピー:
```bash
cp ~/Dropbox/Git/psychometrics_syllabus/該当PDF ~/Dropbox/Git/kosugi-stats/pdfs/
```
3. 必要に応じて index.qmd のバージョン番号を更新
4. ビルド & デプロイ:
```bash
cd ~/Dropbox/Git/kosugi-stats
quarto render
git add -A && git commit -m "PDF更新: ファイル名" && git push
```

### 「サイトを公開して」「デプロイして」と言われたら

```bash
cd ~/Dropbox/Git/kosugi-stats
quarto render
git add -A && git commit -m "サイト更新" && git push
```
GitHub Pages が自動でデプロイする（docs/フォルダ）。

### 「Rコードを追加したい」と言われたら

codes/ の該当ディレクトリにファイルを置き、codes.qmd（または basic.qmd / applied.qmd）にリンクを追記。

### 「バージョン番号を更新したい」と言われたら

手で書かない。`quarto render` の pre-render(`tools/update_versions.py`)が `pdfs/*.pdf` に印字された版数を読んで index.qmd の表を書き直す。PDF を差し替えて render するだけでよい。

### 応用(Dkiso2)の各回のコードの約束(2026-10-12)

- 正本はこのリポジトリの `codes/Dkiso2/`。**テキスト(psychometrics_syllabus の course_materials2/tex)の掲載コードに合わせる**(番号はテキストのコード番号をコメントに書く)
- 各回のコードは冒頭で `source("00_Appendix2_functions.R")`(rstan 版は `_rstan.R`)を読み，Stan ファイルもプロジェクトフォルダ直下から読む。学生は関数集をフォルダに置き Stan ファイルを書けば R コードがそのまま走る
- 関数集を直したら，classroom-autograder の `assets/Dkiso2/` にもコピーして deploy する(採点器が学生コードの前に読み込む)

---

## 次
- PsyStatsPracticals の統合検討
- PDF更新ワークフローの自動化

- **ホームの版数表は手で書かない**: `quarto render` の pre-render で `tools/update_versions.py` が `pdfs/*.pdf` の冒頭の「version X.Y.Z」を読んで index.qmd の表を書き直す(pdftotext が無いマシンでは何もしない)。表に資料を足すときはスクリプトの ROWS に足す。応用の事前準備の関数コードは `codes/Dkiso2/00_Appendix2_functions*.R` を include しているので，関数を直せばページも追随する(2026-10-12)
