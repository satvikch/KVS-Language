import sys
from functools import reduce
from tokenize import tokenize, untokenize, NUMBER, STRING, NAME, OP
from io import BytesIO
import re

def lexer(file_path):
    assert file_path.endswith('.kvs'), "Unsupported file extension"
    result = '['
    with open(file_path, 'r') as f:
        content = f.read()
        tokens = tokenize(BytesIO(content.encode('utf-8')).readline)
        list_value = []
        for toknum, tokval, _, _, _ in tokens:
            if len(tokval) != 0:
                if tokval not in {'\n', 'utf-8', '\t', '"'}:
                    if tokval == '[':
                        list_value.append(tokval)
                        continue
                    elif tokval == ']':
                        list_value.append(tokval)
                        result += ''.join(list_value)
                        list_value = []
                    else:
                        if tokval.startswith('"'):
                            # Remove quotes and add to result with quotes escaped
                            value = tokval[1:-1].replace('"', '\\"')
                            result += f'"{value}",'
                        elif tokval == '!=':
                            result += "'!=',"
                        elif tokval in {')', '(', '{', '}'}:
                            result += f"'{tokval}',"
                        else:
                            result += f'{tokval},'
    result = result.rstrip(',') + ']'
    return result
    
if __name__ == '__main__':
    file_path = sys.argv[1]
    tokens = lexer(file_path)