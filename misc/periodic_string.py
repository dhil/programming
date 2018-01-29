# Given a string determine whether it is periodic

def is_periodic(s):
    i = (s+s).find(s, 1, -1)
    return None if i == -1 else s[:i]

print([is_periodic("ABABAB"), is_periodic("ABBAABBA"), is_periodic("AAAAAAAAAA"), is_periodic("ABCDEF"), is_periodic("ABABEABAB")])
