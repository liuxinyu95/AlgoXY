# -*- perl -*-
$pdf_mode = 5;			# xelatex
$dvi_mode = $postscript_mode = 0;
$do_cd = 1;			# output in same dir as main input

@default_files = ( 'algoxy-zh-cn.tex', 'algoxy-en.tex' );

# This doesn't really work:
#
# add_cus_dep('dot', 'pdf', 0, 'dot2pdf');
# sub dot2pdf {
#     system("dot -T pdf -o \"$_[0].pdf\" \"$_[0].dot\"");
# }
