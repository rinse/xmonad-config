# キーバインド一覧

mod キーは **Alt (mod1)**。`Alt+U → X` は Alt+U を押して離してから X を押すサブマップ。

## アプリ起動・プロンプト

| キー | 動作 |
|---|---|
| `Alt+Ctrl+T` / `Alt+Shift+Enter` | 端末を起動 (x-terminal-emulator) |
| `Alt+P` | shellPrompt (コマンドランチャー) |
| `Alt+Shift+P` | gmrun 起動 (要 gmrun) |
| `Alt+U → H` | Hoogle 検索プロンプト (x-www-browser で開く) |
| `Win+L` | 画面ロック (要 lxlock) |

## ウィンドウ操作

| キー | 動作 |
|---|---|
| `Alt+J` / `Alt+K` | フォーカス移動 下/上 (最小化ウィンドウをスキップ) |
| `Alt+Tab` / `Alt+Shift+Tab` | フォーカス移動 下/上 (既定版) |
| `Alt+Shift+J` / `Alt+Shift+K` | ウィンドウ入替 下/上 |
| `Alt+I` | master へ昇格 (swapMaster) |
| `Alt+Shift+C` | ウィンドウを閉じる |
| `Alt+M` / `Alt+Shift+M` | 最小化 / 最後に最小化したものを復元 |
| `Alt+T` | フロートをタイルに戻す |
| `Alt+ドラッグ` (左/右ボタン) | フロート移動 / リサイズ |

## レイアウト

| キー | 動作 |
|---|---|
| `Alt+Space` / `Alt+Shift+Space` | レイアウト切替 (ResizableTall ⇄ Full) / リセット |
| `Alt+H` / `Alt+L` | master 領域の縮小 / 拡大 |
| `Alt+,` / `Alt+.` | master ウィンドウ数の増減 |
| `Alt+N` | xmonad 内部の refresh (レイアウト再計算) |
| `Alt+B` | xmobar 領域 (struts) の避け/重ねトグル |

## ワークスペース・スクリーン

| キー | 動作 |
|---|---|
| `Alt+1`〜`Alt+9` | ワークスペース 1〜9 へ移動 |
| `Alt+Shift+1`〜`9` | ウィンドウをワークスペース 1〜9 へ送る |
| `Alt+Ctrl+L` / `Alt+Ctrl+H` | 次 / 前のワークスペースへ (全スクリーン同期、12 まで巡回) |
| `Alt+Shift+Ctrl+L` / `H` | ウィンドウごと次 / 前のワークスペースへ |
| `Alt+R` / `Alt+Shift+R` | 次のスクリーンへ / ウィンドウごと次のスクリーンへ |
| `Alt+W` / `Alt+E` (+`Shift`) | スクリーン 1 / 2 へフォーカス (ウィンドウ送り) |
| `Alt+U → I` | スクリーン初期化 (initScreens) |

## ユーティリティ・システム

| キー | 動作 |
|---|---|
| `Alt+U → A` / `X` / `M` | 音量 +10% / −10% / ミュート |
| `Alt+U → V` | pavucontrol 起動 |
| `Alt+U → R` | 強制再描画 (xrefresh)。WSLg でリサイズ後に画面下部が欠けたときに使う |
| `Alt+Q` | xmonad 再起動 (設定リロード) |
| `Alt+Shift+/` | ヘルプ表示 |

## 無効化済みの既定キー

| キー | 理由 |
|---|---|
| `Alt+Shift+Q` (終了) | 誤爆防止のため削除 |
| `Alt+Enter` (swapMaster) | IntelliJ 用に空けてある。`Alt+I` を使う |
