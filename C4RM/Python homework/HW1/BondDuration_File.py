def getBondDuration(y, face, couponRate, m, ppy=1):
    ppy = float(ppy)
    periods = m * ppy
    coupon = (face * couponRate) / ppy
    price = sum([coupon / ((1 + y / ppy) ** (t + 1)) for t in range(int(periods))]) + \
            face / ((1 + y / ppy) ** periods)
    weightedprice = sum([(t + 1) * coupon / ((1 + y / ppy) ** (t + 1)) for t in range(int(periods))]) + \
             periods * face / ((1 + y / ppy) ** periods)
    d = weightedprice / price
    return d
