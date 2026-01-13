jjj = load ("-ascii", "{JJ_BIG_NATURAL_32}-product - max = 2000")
gnu = load ("-ascii", "{GMP_INTEGER}-product - max = 2000")
title ("\n\n\n {JJ_BIG_NATURAL_32} Versus {GMP_INTEGER} - product \n \
      red = jjj        green = gnu",
    'Interpreter', 'none')
xlabel ("Number of Digits x100")
ylabel ("Time (sec)")
hold on

    # Plot final columns of {JJ_BIG_NATURAL_32}
plot (jjj (:, end:end), 'r')
plot (jjj (:, end-1:end), 'r--')
plot (jjj (:, end-2:end), 'r-')

    # Plot final columns of {GMP_INTEGER}
plot (gnu (:, end:end), 'g')
plot (gnu (:, end-1:end), 'g')
plot (gnu (:, end-2:end), 'g')

hold off
