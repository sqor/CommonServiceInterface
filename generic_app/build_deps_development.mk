$(DEPS)/SQ2_CommonServiceInterface:
	git clone -n -- git@github.com:Amplify-Social/SQ2_CommonServiceInterface.git $@
	(cd $@ && git checkout -q master && make DEPS_DIR=$(DEPS_DIR))
