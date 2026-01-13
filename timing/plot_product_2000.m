jjj = load ("-ascii", "{JJ_BIG_NATURAL_32}-product - max = 2000")
gnu = load ("-ascii", "{GMP_INTEGER}-product - max = 2000")
surf (jjj)
title ("{JJ_BIG_NATURAL_32} - Time to multiply two big numbers. \n \ 
    JJ_BIG_NATURAL is many times slower than the gnu feature \n \
    9.5 seconds versus 0.25 seconds",
    'Interpreter', 'none')
xlabel ("Number of Digits x100")
ylabel ("Number of Digits x100")
zlabel ("Time (sec)")
#hold on
#surf (gnu)
#hold off
