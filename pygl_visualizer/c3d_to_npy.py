import os
import sys
import c3d
import argparse
import numpy as np

def convert(file, output):
    assert '.c3d' in file
    assert '.npy' in output
    
    points = []
    analog = []
    reader = c3d.Reader(open(file, 'rb'))
    for i, p, a in reader.read_frames():
        points.append(p)
        analog.append(a)
        
    out_path, ext = os.path.splitext(output)
    np.save(out_path+'_'+'points'+ext, np.array(points))
    np.save(out_path+'_'+'analog'+ext, np.array(analog))
    
def parse_args(args):
    parser.add_argument('-f', dest='file', help='The c3d file you\'d like to make into a npy')
    parser.add_argument('-o', dest='output', help='The output path you want your numpy to be.')
    return parser.parse_args(args)

if __name__ == '__main__':
    args = parse_Args(sys.argv[1:])
    convert(args.file, args.output)
    
'''
author @ yvan
'''
