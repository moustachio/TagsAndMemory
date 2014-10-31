from scipy import sparse
import MySQLdb
import numpy as np

def sparse_saver(mat,filename):
	np.savez_compressed(filename,data=mat.data,indices=mat.indices,indptr=mat.indptr,shape=mat.shape)

mysql_user,mysql_pass = [line.strip() for line in open('mysql.key')]
db = MySQLdb.connect(host="127.0.0.1", user=mysql_user, passwd=mysql_pass,db="analysis_lastfm")
cursor = db.cursor()

n = cursor.execute("select user_id from lastfm_users where scrobbles_recorded = 1;")
users = cursor.fetchall()

mat = np.empty((n,91))
basedate = (2005*12)+7

for i,u in enumerate(users):
	user = u[0]
	cursor.execute("select year(scrobble_time),month(scrobble_time),count(*) from lastfm_scrobbles where user_id=%s  group by year(scrobble_time),month(scrobble_time)",(user,))
	overallMonthlyPlaycounts = np.zeros(91)
	overallMonthlyPlaycounts[0] = user
	for y,m,cnt in cursor.fetchall():				
		month_index = (((12*y)+m) - basedate)
		overallMonthlyPlaycounts[month_index+1] = cnt
	mat[i] = overallMonthlyPlaycounts

sparse_mat = sparse.csr_matrix(mat)
ddir = 'E:/BTSync/Research.Archive/LastFM/TagsAndMemory'
fi = ddir+'totalMonthlyPlaycountsByUser'
sparse_saver(sparse_mat,fi)