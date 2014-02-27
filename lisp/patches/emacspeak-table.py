import re

def parse_latex_table(LatexTable):
    """Parses the body of the latex table into a tuple of tuples for emacspeak table structure"""
    result = []
    cell_Count = []
    heading = re.search(r'\\begin\{tabular\}(\[t\])?\{([clr|]|p\{.*\})*\}',LatexTable)
    if not heading: return 'heading error'
    tail = re.search(r'\\end{tabular\}',LatexTable)
    TableBody = LatexTable[heading.end():tail.start()]
    TableBody = TableBody.replace('\%','PeRcentAge')
    TableBody = comments(TableBody)
    LatexTable = LatexTable.replace( r'\hline', '') 
    TableBody = string_trim(TableBody)
    body_lines = TableBody.split(r'\\')
  
    for line in body_lines:
        line = line_format(line)
        cells = re.split('&',line)
        row = []
        Counter = 0
        for cell in cells:  
            cell = cell_format(cell)
            row.append(str(cell))
            Counter = Counter + 1
        result.append(tuple(row))
        cell_Count.append(Counter)
    final = tuple(result)
    Cell_Count_Set = set(cell_Count)
    if len(Cell_Count_Set) > 1:
        return 'Error: Column count variances', cell_Count 
    return final

def comments(BodyText):
    while '%' in BodyText:
        start = BodyText.find('%')
        Length = len(BodyText)
        comment_start = BodyText[start:Length]
        end = comment_start.find('\n')
        comment = comment_start[:end+1]
        BodyText = BodyText.replace(comment, ' ')
    return BodyText
        
def line_format(line):
    line = line.replace('\&','AmPeRsAND')
    if '\multicolumn' in line:
        while '\multicolumn' in line:
            multi, replace_line = paren_balance(line)
            line = line.replace(multi,replace_line)
    """if  (not line) | (line.isspace()): continue # empty, forget it"""
    return line

def cell_format(cell):
    if '\\noalign' in cell:
        align = remove_table_formatting(cell, '\\noalign')
        cell = cell.replace(align,'')
    if '\\rowcolor' in cell:
        color = remove_table_formatting(cell, '\\rowcolor')
        cell = cell.replace(color,'')
    cell = cell.replace('\hline','')
    cell = cell.replace('\n',' ')
    cell = cell.replace('AmPeRsAND','\&')
    cell = cell.replace('PeRcentAge','\%')
    return cell

def remove_table_formatting(text, format_var):
    output = format_var
    argument = format_var+'{'
    if argument in text:
        align = re.search(r'\{',text)
        string = text[align.end():]
        balanced_align = balance(string)
        output = argument + balanced_align
    else:
        output = format_var
    return output

    
def string_trim(Body_Text):
    """Takes the string for the body of the table and trims off any trailing characters"""
    Body_list = []
    for char in Body_Text:
        Body_list.insert(0, char)
    text = ''.join(Body_list)
    Reversed = re.search(r'\\\\',text)
    Reversed_string = text[Reversed.end():]
    Body_list2 = []
    for char in Reversed_string:
        Body_list2.insert(0, char)
    Trimmed_text = ''.join(Body_list2)
    return Trimmed_text
    

def paren_balance(ParenString):
    """Breaks down the \multicolumn function to return the text and the number of cells to occupy"""
    x = re.search(r'\{([0-9]*)\}', ParenString)
    first_cut = ParenString[x.end()+1:]
    x = int(x.group(1))
    second_cut = balance(first_cut)
    third_cut = first_cut.replace(second_cut,'')
    text = balance(third_cut[1:])
    text = text[:-1]
    replace_line = text + (' & ' + text)*(x-1)
    multi = '\multicolumn{'+str(x)+'}{'+second_cut+'{'+text+'}'
    return multi, replace_line

def balance(MultiCol):
    """Brace balancing function."""
    list_of_output= []
    bracket_count = 1
    for char in MultiCol:
        if char == '{':
            bracket_count = bracket_count + 1
        if char == '}':
            bracket_count = bracket_count - 1
        list_of_output.append(char)
        if bracket_count == 0:
            break
    output_string = "".join(list_of_output)
    return output_string
    
