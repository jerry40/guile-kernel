FROM python:3.9.2
ENV PYTHONUNBUFFERED 1
RUN mkdir /code
WORKDIR /code
RUN sed -i "s/buster/testing/" /etc/apt/sources.list && \
    sed -n -i "/security/!p" /etc/apt/sources.list
RUN cat /etc/apt/sources.list
RUN pip install jupyter
RUN apt-get update && apt-get install -y \ 
    guile-3.0 guile-3.0-dev automake gcc libunwind-dev build-essential
RUN wget https://github.com/zeromq/libzmq/releases/download/v4.3.4/zeromq-4.3.4.tar.gz && \
    tar xvf zeromq-4.3.4.tar.gz && cd /code/zeromq-4.3.4/ && \
    ./autogen.sh && ./configure CXXFLAGS='-Wno-error -Wno-error=stringop-truncation' && make && make install
RUN wget http://download.savannah.gnu.org/releases/guile-json/guile-json-3.2.0.tar.gz && \
    tar xvf guile-json-3.2.0.tar.gz && cd guile-json-3.2.0 && \
    ./configure --prefix=/usr && make && make install
# guile-zimple-zmq
RUN git clone https://github.com/jerry40/guile-simple-zmq.git && \
    cd ./guile-simple-zmq && \
    autoreconf --verbose --install --force && \
    ./configure --prefix=/usr && make && make install
#
RUN mkdir /usr/local/share/jupyter/kernels/guile
WORKDIR /usr/local/share/jupyter/kernels/guile
RUN git clone https://github.com/jerry40/guile-kernel.git
RUN cp ./guile-kernel/src/kernel.json .
EXPOSE 8888
RUN groupadd -r jupyter_users && useradd --no-log-init -m -r -g jupyter_users guile_user
USER guile_user
WORKDIR /home/guile_user
ENTRYPOINT ["jupyter", "notebook", "--ip=0.0.0.0", "--no-browser"]