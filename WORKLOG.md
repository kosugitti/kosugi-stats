# WORKLOG

## 2026-08-01

### stats.kosugitti.net のHTTPS証明書エラーを解消

**症状**: ブラウザで `http://stats.kosugitti.net/` を開くと「安全なサイトではない」警告。ChromeのHTTPS-Firstが自動で `https://` を試し，`NET::ERR_CERT_COMMON_NAME_INVALID` になっていた。

**原因**: GitHub Pages側でこのドメイン用のLet's Encrypt証明書が未発行だった。サーバが返す証明書がGitHubの汎用証明書 `CN=*.github.io`（SANは `*.github.io` 等のみ）で，ホスト名が一致しなかった。

- DNS側は正常（`stats.kosugitti.net` → CNAME → `kosugitti.github.io` → 185.199.108-111.153）
- `kosugitti.net` のCAAも `letsencrypt.org` を許可済みでブロック要因なし
- `CNAME` ファイル（ルート・docs/ 両方）も `stats.kosugitti.net` で正しい
- API上は `cname` は設定済みだが `https_enforced=false`，`https_certificate` フィールド自体が存在しない＝証明書がまったく作られていない状態だった

**対処**: カスタムドメインを解除→再設定して証明書発行を再トリガした。

```bash
gh api -X PUT repos/kosugitti/kosugi-stats/pages -f cname=              # 解除
gh api -X PUT repos/kosugitti/kosugi-stats/pages -f cname=stats.kosugitti.net  # 再設定
# 証明書 NONE → authorization_pending → approved（約20分）
gh api -X PUT repos/kosugitti/kosugi-stats/pages -F https_enforced=true
```

**結果**: `subject=CN=stats.kosugitti.net` / issuer=Let's Encrypt YR1 / 有効期限 2026-10-30 の正しい証明書に置き換わり，HTTPS 200で疎通。`https_enforced=true`。

- ドメイン再設定の直後にPagesビルドが1回 `errored` になったが，直後の再ビルドが `built` で完了。一過性でサイト内容に影響なし
- 実行時点では `http://` からの301リダイレクトが未反映（Fastlyのエッジキャッシュ切れ待ち・設定自体は入っている）

### 他サイトの横断点検（同じ問題がないか確認）

- GitHub Pages有効リポジトリ21件を全チェック。カスタムドメイン使用は2件のみで，`kosugi-labo`（labo.kosugitti.net）は cert=approved・https_enforced=true で元から正常。残り19件は `*.github.io` 既定ドメインのため問題なし
- Pages以外も実接続で検査し全て正常: `kosugitti.net`，`www.kosugitti.net`，`www.psy.senshu-u.ac.jp`，`www3`，`ext`，`gico`，`kujira`，`sv1`，`sv2`

**引き継ぎ**: `wundt.psy.senshu-u.ac.jp` は接続タイムアウトで応答なし（証明書ではなく到達性の問題。学外から不可か停止中）。今回は未着手なので気になるなら別途調査。

## 2026-04-01（続き）
- CNAME追加（stats.kosugitti.net）
- CLAUDE.md にユーザ操作サポート手順を追記

## 2026-04-01
- kosugi-stats リポジトリ新規作成（psychometrics_syllabus の教材サイト移転先）
- Quarto Website として構築（テーマ: cosmo, lang: ja）
- psychometrics_syllabus から PDF 12ファイル、Rコード 43ファイル、サンプルデータ 11ファイルをコピー
- 6ページ構成: index / basic / applied / lectures / codes / about
- GitHub Pages デプロイ完了: https://kosugitti.github.io/kosugi-stats/
- psychometrics_syllabus に kosugi-stats へのリダイレクト設置（index.html + 404.html）

## CLAUDE.mdからの退避 (2026-07-17)

ホーム索引(~/Dropbox/CLAUDE.md)のステータスセル圧縮時の退避(退避時点の全文):

サイト再構成・稼働中。ベイズ章群(b25-b29)シラバス再編完了。ch07/ch08の回帰例を勉強時間・単語数→テスト点に全面改訂(GPA>4の誤り解消，図はggplot2/ragg/scatterplot3d/TikZ化，図8.5-8.8の矢印・配置も調整)。v3.29.3でビルド→kosugi-statsへ反映済。KDP公開版は旧m系のままで修正は全てV3(ch系)側，V3ベースで出し直す方針(夏休みの宿題)。残作業: ch29本文新規執筆(4節・現状白紙で最優先ブロッカー)，ch27本文圧縮(18.5k→10k字)＋章タイトルのシラバス同期，ch28 JASPバージョン更新，KDP用体裁ラッパ(BasicBook3_kdp.tex)移植，m系SEM章新規執筆。夏休みに集中執筆，普段は空き時間に1節ずつ。詳細→Git/kosugi-stats/WORKLOG.md，Git/psychometrics_syllabus/WORKLOG.md
