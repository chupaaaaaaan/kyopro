#!/bin/bash

set -euo pipefail
cd $(dirname $0)

if [ $# -ne 1 ]; then
    echo "Invalid number of argument: $#"
    exit 1
fi

# 引数で与えたURL（引数のURL）
URL_FROM_ARG="$1"
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
    cat "${TEMPLATE_FILE}" > "${SUBMISSION_PATH}"/Main.hs

else
    # 2. 上記以外の場合 =>
    # 2-1. ファイルのURLに対応するパスにMain.hsファイルが存在すれば退避
    if [ -s "${CODEPATH_FROM_FILE}/Main.hs" ]; then
        T=$(stat -c %Y "${CODEPATH_FROM_FILE}"/Main.hs)
        mv "${CODEPATH_FROM_FILE}"/Main.hs "${CODEPATH_FROM_FILE}"/Main-"${T}".hs
    fi

    # 2-2. 現在の提出コードとバンドル済みソースをファイルのURLに対応するパスに移動
    mkdir -p "${CODEPATH_FROM_FILE}"
    mv "${SUBMISSION_PATH}"/Main.hs "${CODEPATH_FROM_FILE}"/Main.hs

    # 2-3. 引数のurlに対応するパスにMain.hsファイルが存在すれば提出コードに移動、存在しなければテンプレートから提出コードを新規作成
    rm -fr "${SUBMISSION_PATH}" && mkdir -p "${SUBMISSION_PATH}"
    if [ -s "${CODEPATH_FROM_ARG}/Main.hs" ]; then
        mv "${CODEPATH_FROM_ARG}"/Main.hs "${SUBMISSION_PATH}"/Main.hs
    else
        cat "${TEMPLATE_FILE}" > "${SUBMISSION_PATH}"/Main.hs
    fi
fi

echo "${URL_FROM_ARG}" > url

rm -fr cases/sample
mkdir -p cases/sample
oj d -d cases/sample "${URL_FROM_ARG}"
