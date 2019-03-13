def find_add(filePath,in_file,query,to_add):

    print('Reading file:'+ in_file)
    with open(filePath+in_file,'r') as ifile: # Reading the file
        buf=ifile.readlines()

    out_file=in_file

    print('Writing file:'+out_file)
    with open(filePath+out_file,'w') as ofile:
        for line in buf:
            if(query in line):
                line=line+to_add+'\n'
            ofile.write(line)

    print('Modifiying file='+in_file+' is finished\n')