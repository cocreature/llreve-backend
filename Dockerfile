FROM ubuntu:16.04
MAINTAINER Moritz Kiefer <moritz.kiefer@purelyfunctional.org>
ENV LLVM_VERSION 3.9.0
ENV Z3_VERSION 4.5.0
RUN apt-get update && \
    apt-get install -y \
      curl \
      cmake \
      g++ \
      gcc \
      libgmp-dev \
      ninja-build \
      python \
      xz-utils && \
    rm -rf /var/lib/apt/lists/*
RUN curl -SL https://github.com/Z3Prover/z3/archive/z3-$Z3_VERSION.tar.gz | tar xz && \
    mkdir /z3-z3-$Z3_VERSION/build && \
    cd /z3-z3-$Z3_VERSION && \
    python contrib/cmake/bootstrap.py create && \
    cd /z3-z3-$Z3_VERSION/build && \
    cmake .. -GNinja \
             -DCMAKE_BUILD_TYPE=Release \
             -DCMAKE_INSTALL_PREFIX=/usr/local \
             -DBUILD_LIBZ3_SHARED=OFF \
             -DUSE_OPENMP=FALSE && \
    ninja && ninja install && \
    rm -r /z3-z3-$Z3_VERSION
RUN curl -SL http://bifunctor.purelyfunctional.org/downloads/llreve -o /usr/bin/llreve && chmod +x /usr/bin/llreve
RUN curl -SL http://bifunctor.purelyfunctional.org/downloads/llreve-dynamic -o /usr/bin/llreve-dynamic && chmod +x /usr/bin/llreve-dynamic
RUN apt-get update && apt-get install -y unzip
RUN curl -SL http://logicrunch.it.uu.se:4096/%7Ewv/eldarica/eldarica-bin-nightly.zip > eldarica.zip && \
    unzip eldarica.zip && \
    cp -r binary-dist/dist binary-dist/eld binary-dist/eldEnv binary-dist/eld-client /usr/local/bin && \
    rm -r binary-dist
RUN apt-get update && apt-get install -y openjdk-8-jre
