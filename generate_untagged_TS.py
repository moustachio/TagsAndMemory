import numpy as np
from scipy import sparse
import os

## Generating MySQL query: select s.user_id,s.artist_id,year(s.scrobble_time),month(s.scrobble_time),count(*) from lastfm_scrobbles s left outer join lastfm_annotations a on s.artist_id = a.artist_id and s.user_id=a.user_id where a.user_id is null group by s.user_id,s.artist_id,year(s.scrobble_time),month(s.scrobble_time) into outfile 'untagged_TS_20141102.tsv';

infile = 'E:/BTSync/Research.Archive/LastFM/TagsAndMemory/untagged_TS.tsv_20141102.tsv'
drive = 'D:/'
filerows = 286901666

nMonths = 90
arrayLength = 2+nMonths
basedate = (2005*12)+7
chunksize=1000000

def sparse_saver(mat,filename):
	np.savez_compressed(filename,data=mat.data,indices=mat.indices,indptr=mat.indptr,shape=mat.shape)

def sparse_loader(filename):
	npz = np.load(filename)
	return sparse.csr_matrix((npz['data'], npz['indices'], npz['indptr']), shape=npz['shape'])


mat = np.empty((chunksize,arrayLength))
current = np.zeros(nMonths)
	
chunk = 0
chunk_i = 0
total_TS = 0

lastUser = None
lastArtist = None

for i,line in enumerate(open(infile)):

	if i%100000==0:
		print '%.2f percent of file processed' % (100*((i+1.)/filerows))
		print '%s total time series processed' % total_TS

	if chunk_i%chunksize==0 and chunk_i>0:
		sparse_mat = sparse.csr_matrix(mat)
		print 'saving chunk %s' % chunk
		fi = drive+'chunk%03d' % chunk
		sparse_saver(sparse_mat,fi)
		mat = np.empty((chunksize,arrayLength))
		chunk+=1
		chunk_i = 0

	user,artist,year,month,freq = map(int,line.strip().split('\t'))
	month_index = (((12*year)+month) - basedate)

	if lastUser and ((user!=lastUser) or (artist!=lastArtist)):
		current = np.concatenate([[lastUser,lastArtist],current])
		mat[chunk_i] = current
		current = np.zeros(nMonths)
		chunk_i += 1
		total_TS += 1

	current[month_index] = freq
	lastUser,lastArtist = user,artist

# Now we need to throw "current" into the matrix
current = np.concatenate([[lastUser,lastArtist],current])
mat[chunk_i] = current


# we initialized each chunk to be 1 million rows, so we have to trim the last one down.
trimmed = mat[~np.all(mat<1,axis=1)]
sparse_mat = sparse.csr_matrix(trimmed)
print 'saving chunk %s' % chunk
fi = drive+'chunk%03d' % chunk
sparse_saver(sparse_mat,fi)

# Now we concatenate all the chunks together into one big sparse matrix.
prefix = drive
files = [f for f in os.listdir(prefix) if '.npz' in f]
data = sparse.csr_matrix((0,arrayLength))
for fi in files:
	data = sparse.vstack([data,sparse_loader(prefix+fi)])
sparse_saver(data,'E:/BTSync/Research.Archive/LastFM/TagsAndMemory/TS_untagged_all')
