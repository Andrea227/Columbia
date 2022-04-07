
import sys
import math


def getBondPrice(y, face, couponRate, m, ppy=1):
    ppy = float(ppy)
    periods = m * ppy
    coupon = (face * couponRate) / ppy
    price = sum([coupon/((1+y/ppy) ** (t+1)) for t in range(int(periods))]) + \
        face / ((1 + y / ppy) ** periods)
    return(price)