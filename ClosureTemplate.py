import StringIO

def escape_html(s):
    if isinstance(s, basestring):
        out = StringIO.StringIO()
        xlat = {
            '<': '&lt;',
            '>': '&gt;',
            '"': '&quot;',
            "'": '&#039;',
            '&': '&amp;',
        }
        for ch in s:
            out.write(xlat.get(ch, ch))
        return out.getvalue()
    else:
        return unicode(s)

def get_attr_or_item(obj, accessor):
    if hasattr(obj, accessor):
        r = getattr(obj, accessor)
    else:
        r = obj[accessor]
    if callable(r):
        r = r()
    return r

