

def getBondPrice_Z(face, couponRate, times, yc):
    coupon = face * couponRate
    final = len(yc)
    price = sum([coupon / ((1 + y) ** t) for (t, y) in zip(times, yc)]) + \
            face / ((1 + yc[final - 1]) ** times[final - 1])
    return price