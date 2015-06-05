## back haskellcn
haskellcn ()
{
   mongoexport --db snap_web_db --collection users -out users.json
   mongoexport --db snap_web_db --collection auth_user -out auth_user.json
   mongoexport --db snap_web_db --collection topics -out topics.json
   mongoexport --db snap_web_db --collection replies -out replies.json
}

## backup nodeclub
backfold=$(date +"%Y-%m-%d")
rm -rf $backfold
mkdir $backfold
mongoexport --db snap_web_db --collection auth_user   -out $backfold/nc-auth-user.json
mongoexport --db snap_web_db --collection users   -out $backfold/nc-users.json
mongoexport --db snap_web_db --collection topics  -out $backfold/nc-topics.json
mongoexport --db snap_web_db --collection replies -out $backfold/nc-replies.json
mongoexport --db snap_web_db --collection tags -out $backfold/nc-tags.json

## TODO Fixme
doimport ()
{
   mongoimport --host 127.0.0.1 --port 27017 --db snap_web_db --collection auth_user   -file  nc-auth-user.json
   mongoimport --host 127.0.0.1 --port 27017 --db snap_web_db --collection users   -file  nc-users.json
   mongoimport --host 127.0.0.1 --port 27017 --db snap_web_db --collection topics  -file  nc-topics.json
   mongoimport --host 127.0.0.1 --port 27017 --db snap_web_db --collection replies -file  nc-replies.json
   mongoimport --host 127.0.0.1 --port 27017 --db snap_web_db --collection tags -file  nc-tags.json
}
