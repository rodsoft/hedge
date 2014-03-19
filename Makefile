TESTS = case1 case2 case3 case4 case5 case6 case7 case8 split_face \
	     split_edge3 split_edge4 split_edge_mid3 split_edge_mid4 \
	     split_edge_new_faces4 \
	     flip_edge4 flip_edge5 double_flip_edge flip_border \
	     remove_inner_vertex remove_border_vertex1 remove_border_vertex2

TESTS_COMP = $(addsuffix .comp,$(TESTS))
TESTS_OUT = $(addsuffix .out,$(TESTS))
TESTS_EXP = $(addsuffix .exp,$(addprefix exp/,$(TESTS)))
TESTS_PS = $(addsuffix .ps,$(addprefix exp/,$(TESTS)))
TESTS_COMP = $(addsuffix .comp,$(TESTS))

test: $(TESTS_COMP)

%.out: hedge.lua
	@lua -e "require 'hedge'.test('$*',io.open('$@','w'))"

%.comp: %.out
	@if diff $< exp/$*.exp > $*.diff; then rm $*.diff; else echo TEST $* FAILED && false; fi


exp/%.exp: %.out
	cp $< $@

exp/%.ps: exp/%.exp
	dot -Tps -o $@ $<

gen_expected: $(TESTS_EXP) $(TESTS_PS)

bench:
	lua -e "require 'hedge'.bench_all()"
