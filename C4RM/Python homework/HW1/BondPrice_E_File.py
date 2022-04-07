

def getBondPrice_E(face, couponRate, m, yc):
    periods = m
    coupon = face * couponRate
    price = sum([coupon / ((1 + y) ** (t + 1)) for (t, y) in enumerate(yc)]) + \
            face / ((1 + yc[m-1]) ** periods)
    return price
