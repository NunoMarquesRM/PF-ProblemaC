target = 24
coins = [1,3,4]


def g( coins, target ) :
    if not target :
        return 0
    for c in coins :
        if  c <= target :
            return 1 + g( coins, target - c )
    return -1

def d( coins, target ) :
    if not target :
        return 0
    t_min = 5000
    for c in coins :
        if c <= target :
            t = 1 + d( coins, target - c )
            if t < t_min :
                t_min = t
    return t_min


# PUT THE LIMITS
# SEE PAPER TO CHECK WHERE TO STOP



def prepare_coins( coins ) : return coins[::-1]

coins = prepare_coins( coins )
print( g( coins, target ) )
print( d( coins, target ) )

flag = False
for i in range( coins[ 0 ] * 3 ) :
    if g( coins, i ) != d( coins, i ) :
        flag = True
        print( i )
        break

if not flag :
    print( "YES" )