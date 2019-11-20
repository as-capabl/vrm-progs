#/bin/sh
SRC=ext/glTF/specification/2.0/schema
DEST=gltf/schema
GLTF_REPO=`(cd $SRC;git remote get-url origin)`
GLTF_HEAD=`(cd $SRC;git rev-parse HEAD)`

if [ $GLTF_HEAD = "" ]
then
    echo "Make sure current directory is repository root"
    echo "and submodules are initialized."
    exit 1
fi

#rsync -av $SRC $DEST
mkdir -p $DEST
cp $SRC/* $DEST/

cat > $DEST/README <<EOS
Files diverted from $GLTF_REPO
commit hash is $GLTF_HEAD
EOS
