FROM fukamachi/qlot:latest

WORKDIR /app

COPY . .

RUN set -x; \
	apt-get update && apt-get -y install \
	git

RUN qlot install
RUN qlot exec sbcl --noinform --eval "(ql:quickload :lem-server)"
ENTRYPOINT qlot exec sbcl --noinform --eval "(ql:quickload :lem-server)" --eval "(ql:quickload :lem-rooms-client)" --eval "(lem-server:run-websocket-server :hostname \"0.0.0.0\")" --quit
