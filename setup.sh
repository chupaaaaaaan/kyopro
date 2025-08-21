#!/bin/bash

set -euo pipefail
cd $(dirname $0)

# parse options
OPTN=0

while getopts n option 2> /dev/null
do
    case ${option} in
        n) while read -p "Create a new submission file? [y/N] " yn; do
               case "${yn}" in
                   [Yy]|[Yy][Ee][Ss])
                       echo "New submission file is created. Continue processing..." 1>&2
                       OPTN=1
                       break
                       ;;
                   *)
                       echo "New submission file is not created. Continue processing..." 1>&2
                       break
                       ;;
               esac
           done
           ;;
        \?)
            echo "Invalid option: '-${option}'" 1>&2
            exit 1
            ;;
    esac
    shift
done

if [ $# -ne 1 ]; then
    echo "Invalid number of argument: $#" 1>&2
    exit 1
fi

# 引数で与えたURL（引数のURL）
export URL_FROM_ARG="$1"
# 引数のURLに対応するパス
CODEPATH_FROM_ARG="code/${URL_FROM_ARG##*://}"
# urlファイルから取得したURL（ファイルのURL）
URL_FROM_FILE="$( [ -s url ] && cat url )"
# ファイルのURLに対応するパス
CODEPATH_FROM_FILE="code/${URL_FROM_FILE##*://}"
# 提出コードのパス
SUBMISSION_PATH=submission
# テンプレートファイル
TEMPLATE_FILE=src/template.hs

if [ -z "${URL_FROM_FILE}" -o ! -e "${SUBMISSION_PATH}/Main.hs" ]; then
    # 1. ファイルのURLが空 or 提出コードが存在しない場合 => テンプレートから提出コードを新規作成
    rm -fr "${SUBMISSION_PATH}" && mkdir -p "${SUBMISSION_PATH}"
    envsubst < "${TEMPLATE_FILE}" > "${SUBMISSION_PATH}"/Main.hs

else
    # 2. 上記以外の場合 =>
    # 2-1. 現在の提出コードをファイルのURLに対応するパスに移動
    mkdir -p "${CODEPATH_FROM_FILE}"
    mv "${SUBMISSION_PATH}"/Main.hs "${CODEPATH_FROM_FILE}"/Main.hs

    # 2-2. オプション'-n'が指定され、かつ引数のURLに対応するパスにMain.hsファイルが存在すれば退避
    if [ ${OPTN} -eq 1 -a -s "${CODEPATH_FROM_ARG}/Main.hs" ]; then
        T=$(stat -c %Y "${CODEPATH_FROM_ARG}"/Main.hs)
        mv "${CODEPATH_FROM_ARG}"/Main.hs "${CODEPATH_FROM_ARG}"/Main-"${T}".hs
    fi

    # 2-3. 引数のurlに対応するパスにMain.hsファイルが存在すれば提出コードに移動、存在しなければテンプレートから提出コードを新規作成
    rm -fr "${SUBMISSION_PATH}" && mkdir -p "${SUBMISSION_PATH}"
    if [ -s "${CODEPATH_FROM_ARG}/Main.hs" ]; then
        mv "${CODEPATH_FROM_ARG}"/Main.hs "${SUBMISSION_PATH}"/Main.hs
    else
        envsubst < "${TEMPLATE_FILE}" > "${SUBMISSION_PATH}"/Main.hs
    fi
fi

echo "${URL_FROM_ARG}" > url

## TODO 2-3で現在の提出コードと異なるMain.hsを移動する場合、一旦編集し（てタイムスタンプを更新し）ないと（ほぼ間違いなく）テストに失敗してしまう
## バンドルする際に、以下パスの古い提出コード（の一時ファイル）を見るため
## このスクリプトで削除するのはビミョーなので、bundler側で対処したい
rm -fr /tmp/kyopro_lib_PreProcessed/LOCAL/submission/*

rm -fr cases/sample
mkdir -p cases/sample
oj d -d cases/sample "${URL_FROM_ARG}"
