base_dir ?= $(abspath .)
lib_dir = $(base_dir)/lib
framework_dir = $(base_dir)/dsp-framework
ivy_dir = $(base_dir)/.ivy2
ROCKETCHIP_DIR=$(framework_dir)/rocket-chip
TESTCHIPIP_DIR=$(framework_dir)/testchipip

default: mems

SBT ?= java -XX:+CMSClassUnloadingEnabled -Xmx8G -Xss128M -jar $(ROCKETCHIP_DIR)/sbt-launch.jar -Dsbt.ivy.home="${ivy_dir}"

include $(framework_dir)/Makefrag

FIRRTL_JAR ?= $(ROCKETCHIP_DIR)/firrtl/utils/bin/firrtl.jar
FIRRTL ?= java -Xmx2G -Xss8M -cp $(FIRRTL_JAR) firrtl.Driver

CHISEL_ARGS ?= 
build_dir ?= $(base_dir)/generated-src
PROJECT ?= craft
MODEL ?= DspTop
CFG_PROJECT ?= power
CONFIG ?= DefaultStandaloneFixedPointPFBConfig
VLSITOP ?= PFBBlock

long_name = $(PROJECT).$(MODEL).$(CONFIG)

MEM_GEN ?= $(base_dir)/vlsi/src/vlsi_mem_gen

$(build_dir)/$(long_name).fir: $(call lookup_scala_srcs, $(base_dir)/src) $(all_stamps)
	mkdir -p $(build_dir)
	cd $(base_dir) && $(SBT) "run-main $(PROJECT).Generator $(CHISEL_ARGS) $(build_dir) $(PROJECT) $(MODEL) $(CFG_PROJECT) $(CONFIG)"

$(build_dir)/$(long_name).v $(build_dir)/$(long_name).harness.v $(build_dir)/$(long_name).conf: $(build_dir)/$(long_name).fir $(FIRRTL_JAR)
	cd $(base_dir) && $(SBT) "run-main barstools.tapeout.transforms.GenerateTopAndHarness -i $< --top-o $(build_dir)/$(long_name).v --harness-o $(build_dir)/$(long_name).harness.v --syn-top $(VLSITOP) --harness-top $(MODEL) --seq-mem-flags \"-o:$(build_dir)/$(long_name).conf\" --list-clocks \"-o:$(build_dir)/$(long_name).domains\""

$(build_dir)/$(long_name).mems.v $(build_dir)/mems.behav.v: $(build_dir)/$(long_name).conf $(MEM_GEN)
	cd $(build_dir) && $(MEM_GEN) --conf $(long_name).conf --v $(long_name).mems.v --generate --behav mems.behav.v --ipxact "$(wildcard $(build_dir)/*.xml)"

firrtl: $(build_dir)/$(long_name).fir
verilog: $(build_dir)/$(long_name).v
mems: $(build_dir)/$(long_name).mems.v

test: $(all_stamps)
	$(SBT) test

travis: $(all_stamps)
	$(SBT) travis:test

pages: $(all_stamps)
	$(SBT) ghpagesPushSite

clean-libs:
	rm -rf $(lib_dir) $(ivy_dir)/local

clean:
	rm -rf $(build_dir)
