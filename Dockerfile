FROM python:3
ENV PYTHONUNBUFFERED 1
RUN mkdir /code
WORKDIR /code
RUN apt-get update && apt-get install -y \ 
    guile-2.0-dev guile-2.0 automake gcc libunwind-dev
RUN wget https://github.com/zeromq/libzmq/releases/download/v4.3.2/zeromq-4.3.2.tar.gz && \
    tar xvf zeromq-4.3.2.tar.gz && cd /code/zeromq-4.3.2/ && \
    ./autogen.sh && ./configure CXXFLAGS='-Wno-error -Wno-error=stringop-truncation' && make && make install
RUN wget http://download.savannah.gnu.org/releases/guile-json/guile-json-3.2.0.tar.gz && \
    tar xvf guile-json-3.2.0.tar.gz && cd guile-json-3.2.0 && \
    ./configure --prefix=/usr && make && make install
RUN cd /usr/share/guile/site && \
    wget https://raw.githubusercontent.com/jerry40/guile-simple-zmq/master/src/simple-zmq.scm
RUN pip install jupyter
RUN mkdir /usr/local/share/jupyter/kernels/guile
WORKDIR /usr/local/share/jupyter/kernels/guile
RUN git clone https://github.com/jerry40/guile-kernel.git
RUN cp ./guile-kernel/src/kernel.json .
EXPOSE 8888
RUN groupadd -r jupyter_users && useradd --no-log-init -m -r -g jupyter_users guile_user
USER guile_user
WORKDIR /home/guile_user
ENTRYPOINT ["jupyter", "notebook", "--ip=0.0.0.0", "--no-browser"]