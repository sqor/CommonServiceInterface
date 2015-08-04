PROJECT = em
DEPS = csi

dep_csi = git git@github.com:Amplify-Social/SQ2_CommonServiceInterface.git master

include erlang.mk

# Compile flags
# if you want to have lager used by CSI include -Dlager
# 
ERLC_COMPILE_OPTS= -Dlager

# Use the same settings for compiling releases as well as for testing
ERLC_OPTS= $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS= $(ERLC_COMPILE_OPTS)
