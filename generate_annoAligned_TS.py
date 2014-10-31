import numpy as np
from scipy import sparse
import MySQLdb

mysql_user,mysql_pass = [line.strip() for line in open('mysql.key')]
db = MySQLdb.connect(host="127.0.0.1", user=mysql_user, passwd=mysql_pass,db="analysis_lastfm")
cursor = db.cursor()

users = {}

## Generating MySQL query: select a.* from lastfm_annotations a join lastfm_users u on a.user_id=u.user_id where u.scrobbles_recorded=1 order by user_id,artist_id,tag_id,tag_month into outfile X;''
infile = 'E:/BTSync/Research.Archive/LastFM/TagsAndMemory/anno_scrobbleUsers_uid-aid-tid-date_20141029.tsv'
filerows = 29121682

nMonths = 90
arrayLength = 4+(2*nMonths)-1
basedate = (2005*12)+7
chunksize=1000000

def sparse_saver(mat,filename):
	np.savez_compressed(filename,data=mat.data,indices=mat.indices,indptr=mat.indptr,shape=mat.shape)

def sparse_loader(filename):
	npz = np.load(filename)
	return sparse.csr_matrix((npz['data'], npz['indices'], npz['indptr']), shape=npz['shape'])


mat = np.empty((chunksize,arrayLength))
chunk = 0

for i,line in enumerate(open(infile)):

	if i>0 and i%chunksize==0:
		sparse_mat = sparse.csr_matrix(mat)
		print 'saving chunk %s' % chunk
		fi = 'E:/chunk%03d' % chunk
		sparse_saver(sparse_mat,fi)
		mat = np.empty((chunksize,arrayLength))
		chunk+=1

	current = np.zeros(nMonths)

	user,item,artist,tag,date = line.strip().split('\t')
	print '%s / %s, %.2f percent complete' % (i+1,filerows,100*((i+1.)/filerows))
	year,month = date.split('-')[:2]
	month_index = (((12*int(year))+int(month)) - basedate)

	cursor.execute("select year(scrobble_time),month(scrobble_time),count(*) from lastfm_scrobbles where user_id=%s and artist_id=%s group by year(scrobble_time),month(scrobble_time)",(user,artist))
	for y,m,cnt in cursor.fetchall():				
		month_index = (((12*y)+m) - basedate)
		current[month_index] = cnt

	current = np.concatenate([[0]*(nMonths-month_index-1),current,[0]*(month_index)])
	# So, to get values corresponding to "real" time periods in aligned time series X, we would use:
	# X[4+nMonths-month_index-1:][:nMonths] 
	current = np.concatenate([map(int,[user,artist,tag,month_index]),current])

db.close()

# we initialized each chunk to be 1 million rows, so we have to trim the last one down.
trimmed = mat[~np.all(mat<1,axis=1)]
sparse_mat = sparse.csr_matrix(trimmed)
print 'saving chunk %s' % chunk
fi = 'E:/chunk%03d' % chunk
sparse_saver(sparse_mat,fi)
mat = np.empty((chunksize,arrayLength))

# Now we concatenate all the chunks together into one big sparse matrix.
prefix = 'E:/'
files = [f for f in os.listdir(prefix) if '.npz' in f]
data = sparse.csr_matrix((0,arrayLength))
for fi in files:
	data = sparse.vstack([data,sparse_loader(prefix+fi)])
sparse_saver(data,'E:/BTSync/Research.Archive/LastFM/TagsAndMemory/TS_annoAligned_all')
