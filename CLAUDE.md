# kosugi-stats プロジェクト

## 概要
- **公開URL**: https://kosugitti.github.io/kosugi-stats/ (サブドメイン: stats.kosugitti.net)
- **ローカル**: `~/Dropbox/Git/kosugi-stats/`
- **形式**: Quarto Website (GitHub Pages, docs/デプロイ)
- **用途**: 心理統計教育教材ポータル（psychometrics_syllabus の後継）
- **リポジトリ**: kosugitti/kosugi-stats (public)

## 状態
- 公開済み、稼働中

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
│   ├── Dkiso1/        # 基礎用Rコード
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

index.qmd のバージョン情報テーブルを編集:
```
| 基礎シラバス | 3.0.1 |
| 基礎テキスト | 3.29.0 |
...
```

---

## 次
- PsyStatsPracticals の統合検討
- PDF更新ワークフローの自動化
