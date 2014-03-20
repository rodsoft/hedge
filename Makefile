TESTS = case1 case2 case3 case4 case5 case6 case7 case8 \
	     triangulate3 triangulate4 triangulate5 \
	     remove_edge remove_border_edge remove_disconnected_edge \
	     remove_semi_connected_edge \
	     add_edge_prev_next add_edge_prev \
	     add_disconnected_edge add_semi_connected_edge \
	     split_face \
	     split_edge3 split_edge4 split_edge_mid3 split_edge_mid4 \
	     flip_edge4 flip_edge5 double_flip_edge flip_border \
	     remove_inner_vertex remove_border_vertex1

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
