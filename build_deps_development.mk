$(DEPS)/lager:
	git clone -n -- https://github.com/basho/lager.git $@
	(cd $@ && git checkout -q 3.0.1 && ./rebar deps_dir="$(DEPS_DIR)" get-deps compile)
