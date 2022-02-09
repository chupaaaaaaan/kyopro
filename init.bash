#!/bin/bash

cd $(dirname $0)

PACKAGE_ENV='package.local'

rm -f ${PACKAGE_ENV}

ghcup set ghc 8.8.4

cabal update

cabal install --package-env ${PACKAGE_ENV}.orig --lib \
      QuickCheck-2.13.2 \
      array-0.5.4.0 \
      attoparsec-0.13.2.3 \
      bytestring-0.10.10.0 \
      containers-0.6.2.1 \
      deepseq-1.4.4.0 \
      extra-1.7.1 \
      fgl-5.7.0.2 \
      hashable-1.3.0.0 \
      heaps-0.3.6.1 \
      integer-logarithms-1.0.3 \
      lens-4.19.1 \
      massiv-0.5.1.0 \
      mono-traversable-1.0.15.1 \
      mtl-2.2.2 \
      mutable-containers-0.3.4 \
      mwc-random-0.14.0.0 \
      parallel-3.2.2.0 \
      parsec-3.1.14.0 \
      primitive-0.7.0.1 \
      psqueues-0.2.7.2 \
      random-1.1 \
      reflection-2.1.5 \
      repa-3.4.1.4 \
      template-haskell-2.15.0.0 \
      text-1.2.4.0 \
      tf-random-0.5 \
      transformers-0.5.6.2 \
      unboxing-vector-0.1.1.0 \
      unordered-containers-0.2.10.0 \
      utility-ht-0.0.15 \
      vector-0.12.1.2 \
      vector-algorithms-0.8.0.3 \
      vector-th-unbox-0.2.1.7

cat ${PACKAGE_ENV}.orig | sed "/bytestring-0.10.10.1/d" > ${PACKAGE_ENV}

rm -f ${PACKAGE_ENV}.orig
