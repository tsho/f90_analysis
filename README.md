#f90_analysis


##make.energy_conversion.sh .f90
大気の気候平均嘛と偏差場間のエネルギー変換量を計算するfortranファイルと
そのfotranファイルのinputファイル、変数を指定するための
bashスクリプトである。


##make.waf.sh .f90
波活動度 Flux を計算するfortranファイルと
そのfotranファイルのinputファイル、変数を指定するための
bashスクリプトである。


##make.eof.sh .f90
主成分解析 (経験直行関数)を計算するfortranファイルと、
そのfortranファイルを実行するためのbashスクリプトである。
このプログラムを実行するためには、
LAPACKが必要である。


##object
上記ファイル群を動かすために必要なサブルーチンがある。


##def
各インプットファイルに必要な設定ファイル。
例、各再解析データの鉛直方向の気圧レベル等。

