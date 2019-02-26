# Enter your code here. Read input from STDIN. Print output to STDOUT

import re
def is_valid(st):
    if st[0] not in ['4', '5', '6']: ## start with 4,5,6
        return False
    elif re.search('^\\d{16}$', st.replace('-', '')) is None: ## exactly 16 digits
        return False
    elif '-' in st and any([len(i)!=4 for i in st.split('-')]): ## 4 digits separated by '-'
        return False
    elif re.search('(\\d)\\1{3}', st.replace('-', '')): ## 4 or more consecutive digits
        return False
    else:
        return True

n = int(input())
while(n > 0):
    val = is_valid(input())
    if val:
        print('Valid')
    else:
        print('Invalid')
    n = n - 1

