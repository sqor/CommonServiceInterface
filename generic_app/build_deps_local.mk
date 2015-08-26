$(DEPS)/SQ2_CommonServiceInterface:
	cp -RH "$(LOCAL_REPOS)/$(subst $(DEPS)/,,$@)" "$@"
	(cd $@ && make DEPS_DIR=$(DEPS_DIR))
