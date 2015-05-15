# effect size calculator
# Calculate effect sizes for given statistics
# written by P.C. Clasen 02.22.12

import math 

response = ""

while response != "n":
 
    test = int(raw_input("""
    Enter the number corresonding to the test
    statistic for which you want an effect size?
    1: T
    2: Z
    3: F with 1 numerator df
    4: F with >1 numerator df
    5: ChiSqr with 1 numerator df
    6: ChiSqr with >1 numerator df
    7: convert r to cohen's d
    8: covert cohen's d to r
    Number: """))

    if test == 1:
        t_stat = float(raw_input("\nWhat is your t-statistic? "))
        df = float(raw_input("How many degrees of freedom? "))

        r_value = round(math.sqrt(t_stat**2/((t_stat**2)+df)), 3)
        d_value = round((2*t_stat)/math.sqrt(df), 3)

        print '\nEffect size r:', r_value 
        print "Cohen's d:", d_value

    elif test == 2:
        z_stat = float(raw_input("\nWhat is your z-statistic? "))
        sample = float(raw_input("What is your sample size (N)? "))

        r_value = round(math.sqrt((z_stat**2)/((z_stat**2)+sample)), 3)
        d_value = round((2*z_stat/math.sqrt(sample)),3)

        print '\nEffect size r:', r_value 
        print "Cohen's d:", d_value

    elif test == 3:
        f_stat = float(raw_input("\nWhat is your F-statistic? "))
        df = float(raw_input("How many denominator degrees of freedom? "))

        r_value = round(math.sqrt(f_stat/(f_stat+df)), 3)
        d_value = round(2*(math.sqrt(f_stat/df)), 3)

        print '\nEffect size r:', r_value 
        print "Cohen's d:", d_value

    elif test == 4:
        f_stat = float(raw_input("\nWhat is your F-statistic? "))
        df_n = float(raw_input("How many numerator degrees of freedom? "))
        df_d = float(raw_input("How many denominator degrees of freedom? "))

        r_value = round(math.sqrt((df_n*f_stat)/((df_n*f_stat)+df_d)), 3)
        d_value = round(2*(math.sqrt((df_n*f_stat)/df_d)), 3)

        print '\nEffect size r:', r_value 
        print "Cohen's d:", d_value   

    elif test == 5:
        chi_stat = float(raw_input("\nWhat is your Chi square statistic? "))
        sample = float(raw_input("What is your sample size (N)? "))

        r_value = round(math.sqrt(chi_stat/sample), 3)
        d_value = round(2*math.sqrt(chi_stat/(sample-chi_stat)), 3)

        print '\nEffect size r:', r_value 
        print "Cohen's d:", d_value

    elif test == 6:
        chi_stat = float(raw_input("\nWhat is your Chi square statistic? "))
        sample = float(raw_input("What is your sample size (N)? "))

        r_value = round(math.sqrt(chi_stat/(chi_stat+sample)), 3)
        d_value = round(2*math.sqrt(chi_stat/(sample)), 3)

        print '\nEffect size r:', r_value 
        print "Cohen's d:", d_value

    elif test == 7:
        r_stat = float(raw_input("\nWhat is your r statistic? "))

        d_value = round(math.sqrt((4*(r_stat**2))/(1-(r_stat**2))), 3)

        print "\nCohen's d:", d_value

    elif test == 8:
        d_stat = float(raw_input("\nWhat is your cohen's d? "))

        r_value = round(math.sqrt((d_stat**2)/(4+(d_stat**2))), 3)

        print '\nEffect size r:', r_value

    response = raw_input("\nWould you like to do another (y/n)?")
