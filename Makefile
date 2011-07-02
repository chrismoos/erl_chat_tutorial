ERL=erl
EBIN=./ebin

all: 
	mkdir -p ${EBIN}
	cp lib/*/*.beam ebin
	cd src && make

erl_chat: all
	${ERL} -smp auto +A 64 +K true  -env ERL_MAX_ETS_TABLES 20000 -boot start_sasl -run chat_server startapp -pa ebin

clean:
	rm -rf ${EBIN}/*.beam