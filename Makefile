steps := \
	step0_repl \
	step1_read_print \
	step2_eval \
	step3_env \
	step4_if_fn_do \
	step5_tco \
	step6_file \
	step7_quote \
	step8_macros \
	step9_try

.PHONY: all clean run
all: $(steps)

define STEP_template =
.PHONY: bin/$(1)
bin/$(1): src/$(1).adb
	STEP=$(1) alr build
endef

$(foreach step,$(steps),$(eval $(call STEP_template,$(step))))

.PHONY: bin/stepA_mal
bin/stepA_mal: src/stepa_mal.adb
	STEP=stepa_mal alr build -- -o stepA_mal

run:
	alr build
	@bin/$(STEP)

.PHONY: steps.diff
steps.diff:
	diff -u step0_*.adb step1_*.adb || true
	diff -u step1_*.adb step2_*.adb || true
	diff -u step2_*.adb step3_*.adb || true
	diff -u step3_*.adb step4_*.adb || true
	diff -u step4_*.adb step5_*.adb || true
	diff -u step5_*.adb step6_*.adb || true
	diff -u step6_*.adb step7_*.adb || true
	diff -u step7_*.adb step8_*.adb || true
	diff -u step8_*.adb step9_*.adb || true
	diff -u step9_*.adb stepA_*.adb || true
