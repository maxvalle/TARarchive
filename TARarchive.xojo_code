#tag Class
Protected Class TARarchive
	#tag Method, Flags = &h0
		Function Append(f as folderItem) As boolean
		  
		  // append a file or a directory (recursive) to the archive
		  
		  dim tempHeader as TARheader
		  dim lastHeader as TARheader
		  dim lastLongHeader as TARlongHeader
		  dim lastLongLongHeader as TARlongLongHeader
		  dim longName as string
		  dim lastFileSize as integer
		  dim reminder as integer
		  dim endBlock as TARblock
		  dim retValue as boolean
		  
		  // check the file to add is valid
		  if f = nil or not f.exists or not f.isReadable then
		    raiseError kErrorFileInvalid
		    return false
		  end if
		  
		  // check the TAR file is valid
		  if file = nil or not file.isWriteable then
		    raiseError kErrorArchiveInvalid
		    return false
		  end if
		  
		  // check if the TAR file already exists
		  // in that case seek the stream to the last block written
		  if file.exists then
		    
		    // open as binary for read/write
		    tarStream = file.OpenAsBinaryFile(true)
		    
		    // check the stream is valid
		    if tarStream = nil then
		      raiseError kErrorArchiveInvalidStream
		      return false
		    end if
		    
		    // retrieve the position of last header in file
		    // and seek. This is where the last file in archive starts
		    tarStream.position = headIndex(ubound(headIndex))
		    
		    // get the header from file
		    lastHeader.StringValue(false) = tarStream.read(512)
		    
		    // check if this is a normal header or a long header
		    if isLongHeader(lastHeader) then // we have a long header
		      
		      // retrieve the long name header
		      longName = tarStream.read(512)
		      
		      // retrieve the next header
		      tempHeader.stringValue(false) = tarStream.read(512)
		      
		      // check again to see if we have a long-long or just long header
		      if isLongHeader(tempHeader) then // a long-long header
		        
		        lastLongLongHeader.linkHeader = lastHeader
		        lastLongLongHeader.longLinkName = longName
		        lastLongLongHeader.longFileHeader.linkHeader = tempHeader
		        lastLongLongHeader.longFileHeader.longName = tarStream.read(512)
		        lastLongLongHeader.longFileHeader.regularHeader.stringValue(false) = tarStream.read(512)
		        lastFileSize = val("&o"+lastLongLongHeader.longFileHeader.regularHeader.fsize)
		        
		      else // a long header
		        
		        lastLongHeader.linkHeader = lastHeader
		        lastLongHeader.longName = longName
		        lastLongHeader.regularHeader = tempHeader
		        lastFileSize = val("&o"+lastLongHeader.regularHeader.fsize)
		        
		      end if
		      
		    else // a standard header. get the archived file size
		      
		      lastFileSize = val("&o"+lastHeader.fsize)
		      
		    end if
		    
		    // seek at the end of the last file in archive, keeping in account
		    // the 512 bytes block padding
		    tarStream.position = tarStream.position + lastFileSize
		    reminder = lastFileSize mod 512
		    if reminder > 0 then
		      tarStream.position = tarStream.position + (512-reminder)
		    end if
		    
		  else
		    
		    // tar file doesn't exists - create it
		    tarStream = file.CreateBinaryFile("")
		    
		    // check the stream is valid
		    if tarStream = nil then
		      raiseError kErrorArchiveInvalidStream
		      return false
		    end if
		    
		  end if
		  
		  // invoke the recursive adding function passing the initial file and the start path (blank)
		  retValue = doAppend(f, "")
		  
		  // write a 512 bytes block full of nil to mark end of TAR file and  close the stream
		  tarStream.write endBlock.StringValue(false)
		  tarStream.close
		  
		  // doAppend already handle errors
		  if retValue then
		    raiseError kErrorNoError
		  end if
		  return retValue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function checkHeader(header as TARheader) As boolean
		  
		  // check header validity
		  
		  dim oldCheckSum as integer
		  
		  // check the magic signature
		  if left(header.magic, 5) <> kHeaderMagic then
		    raiseError kErrorHeaderInvalid
		    return false
		  end if
		  
		  // check the checksum validity
		  if not ignoreChecksumErrors then
		    oldCheckSum = val("&o"+header.checksum)
		    header.checksum = "        "
		    if CheckSum(header.StringValue(false)) <> oldCheckSum then
		      raiseError kErrorHeaderChecksumBad
		      return false
		    end if
		  end if
		  
		  return true
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function CheckSum(block as string) As integer
		  
		  // calculate the checksum of a given string (header)
		  
		  dim i as integer
		  dim s as integer = lenb(block)-1
		  dim sum as integer
		  dim m as memoryBlock
		  
		  m = newMemoryBlock(s+1)
		  m.StringValue(0, s+1) = block
		  for i = 0 to s
		    sum = sum + m.byte(i)
		  next
		  
		  return sum
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function cleanArchiveName(name as string) As string
		  return replaceAll(name, "/", ":")
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function cleanExtractName(name as string) As string
		  #if targetMacOS
		    return replaceAll(name, ":", "/")
		  #else
		    
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(targetFile as folderItem, deferOpen as boolean = false)
		  
		  // init values
		  kNULLOSType = chr(0)+chr(0)+chr(0)+chr(0)
		  
		  // store  the TAR file reference
		  file = targetFile
		  
		  // check the file is not NIL
		  if file = nil then
		    raiseError kErrorArchiveInvalid // file is NIL
		  end if
		  
		  // open the archive if needed
		  if not deferOpen then
		    call self.Open()
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function createTree(path as string, startingPoint as folderItem) As folderItem
		  
		  // check and create a directory tree for a given path (ex. /my/path/to/something/)
		  
		  dim branch() as string
		  dim pathTree as string
		  dim currentFolder as folderItem
		  dim i, n as integer
		  
		  // check for void path
		  if path = "" or path = "./" then
		    return startingPoint
		  end if
		  
		  // remove the trailing slash
		  pathTree = left(path, len(path)-1)
		  
		  // sanitize the starting path
		  if left(pathTree,3) = "../" then
		    pathTree = mid(pathTree, 4)
		  elseif left(pathTree,2) = "./" then
		    pathTree = mid(pathTree, 3)
		  elseif left(pathTree,1) = "/" then
		    pathTree = mid(pathTree, 2)
		  end if
		  
		  // check the path cache if we have already stored this path
		  if pathsCache.hasKey(pathTree) then
		    return FolderItem(pathsCache.value(pathTree)) // already done, just return it
		  end if
		  
		  // At this point we have a new path to create
		  
		  // create an array with the path tree
		  branch = split(pathTree,"/")
		  n = ubound(branch)
		  
		  // check the path tree is not empty
		  if ubound(branch) = -1 then
		    raiseError kErrorTargetCantBeCreated
		    return nil
		  end if
		  
		  // Start from current dir and parse the branch array
		  // to create (and check) all inner sub paths
		  currentFolder = startingPoint
		  for i = 0 to n
		    currentFolder = currentFolder.child(cleanExtractName(branch(i)))
		    if not currentFolder.exists then
		      currentFolder.createAsFolder
		      if not currentFolder.exists then
		        raiseError kErrorTargetCantBeCreated
		        return nil
		      end if
		    end if
		  next
		  
		  // store the newly created path to the cache for later retrieval
		  pathsCache.value(pathTree) = currentFolder
		  
		  // return the path as a folderItem
		  return currentFolder
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function DateToUNIXtimestamp(d as date) As double
		  dim diffDate as new date
		  
		  diffDate.day = 1
		  diffDate.month = 1
		  diffDate.year = 1970
		  diffDate.hour = 0 + diffDate.gMTOffset
		  diffDate.minute = 0
		  diffDate.second = 0
		  
		  return d.totalSeconds - diffDate.totalSeconds
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function doAppend(f as folderItem, startPath as string, isAppleDouble as boolean = false) As boolean
		  
		  // recursively add files and directories to the archive
		  
		  dim i as integer
		  dim fileNameCleaned as string
		  dim fileHeader as string
		  dim isSymLink as boolean
		  dim block as TARblock
		  dim fileStream as BinaryStream
		  dim fileType as TARfileType
		  dim headPosition as integer
		  dim resStream as ResStreamMBS
		  dim AppleDoubleHeaderBlock as string
		  
		  // check if the file is valid
		  if f = nil or not f.exists then
		    raiseError kErrorFileInvalid
		    return false
		  end if
		  
		  // check if the file is readable
		  if not f.isReadable then
		    raiseError kErrorFileNotReadable
		    return false
		  end if
		  
		  // check if the archive stream is valid
		  if tarStream = nil then
		    raiseError kErrorArchiveInvalidStream
		    return false
		  end if
		  
		  // clean and store the file name
		  fileNameCleaned = cleanArchiveName(f.name)
		  
		  // get the header for the file
		  fileHeader = makeTARHeader(f, startPath, isAppleDouble)
		  
		  // check the returned header is not empty
		  if fileHeader = "" then
		    // error handled by MakeHeader - just return false
		    return false
		  end if
		  
		  // get the file type from header
		  fileType = TARfileType(ascB(midb(fileheader, 157, 1)))
		  
		  // check if this is a symlink
		  if fileType = TARfileType.ArchivedLink or fileType = TARfileType.SymbolicLink or fileType = TARfileType.LongNameSymbolicLink then
		    isSymLink = true
		  end if
		  
		  // save the actual position in tar stream
		  headPosition = tarStream.position
		  
		  // check if the file to archive is a directory
		  if f.directory then
		    
		    // Handle Mac content
		    if isAppleDouble then
		      
		      #if TargetMacOS
		        // create the AppleDoubleHeader
		        AppleDoubleHeaderBlock = makeADHeader(f)
		        
		        // write the TAR file header
		        tarStream.write fileHeader
		        
		        block.data = AppleDoubleHeaderBlock
		        tarStream.write block.stringValue(false)
		        
		        // store the file name in file list
		        content.append startPath+kHeaderResourcePrefix+fileNameCleaned
		        
		        // store the position of the header in archive
		        headIndex.append headPosition
		      #endif
		      
		    else // the folder
		      
		      // write the file header
		      tarStream.write fileHeader
		      
		      // store the position of the header in archive
		      headIndex.append headPosition
		      
		      // store the directory name in file list
		      content.append startPath+fileNameCleaned+"/"
		      
		      // call me again for every file contained in this directory
		      for i = 1 to f.count
		        if not doAppend(f.trueItem(i), startPath+fileNameCleaned+"/") then
		          // error handled by nested instances of me
		          return false
		        end if
		      next
		      
		      #if TargetMacOS
		        // check if we need to take in account Mac content (finder info)
		        if not ignoreMacContent then
		          // then check if the folder has special Finder attributes
		          if f.GetFolderFlagsMBS <> 0 then
		            // call again myself to handle the Mac content
		            return doAppend(f, startPath, true)
		          end if
		        end if
		      #endif
		      
		    end if
		    
		  elseif isSymLink then // it' a symbolic link
		    
		    // we have nothing to store for a symbolic link
		    
		    // just write the file header
		    tarStream.write fileHeader
		    
		    // store the position of the header in archive
		    headIndex.append headPosition
		    
		    // store the file name in file list
		    content.append startPath+fileNameCleaned
		    
		  else // it's a file
		    
		    // Handle Mac content and data forks
		    if isAppleDouble then
		      
		      #if TargetMacOS
		        // create the AppleDoubleHeader
		        AppleDoubleHeaderBlock = makeADHeader(f)
		        
		        // write the TAR archive (except for resourceFork that will be written block by block)
		        
		        // write the TAR file header
		        tarStream.write fileHeader
		        
		        // Add resourceFork if needed
		        if f.resourceForkLength <> 0 then
		          // open the resource fork for reading
		          resStream = f.OpenAsResStreamMBS(false)
		          
		          if resStream = nil then
		            raiseError kErrorFileInvalidResourceStream
		            return false
		          end if
		          
		          // Actually we only handle File flags and resources. Since the file flags header + data is < 512 (a block)
		          // calculate the AppleDouble header + entry headers size and
		          // fill the block with these + a part of the resourceFork to completelly fill a block
		          // and write the first block
		          block.data = AppleDoubleHeaderBlock + resStream.read(512-lenb(AppleDoubleHeaderBlock))
		          tarStream.write block.stringValue(false)
		          
		          // copy the file into the archive one block at time
		          while not resStream.EOF
		            block.data = resStream.read(512)
		            tarStream.write block.StringValue(false)
		          wend
		          
		          // close the resource stream
		          resStream.close
		          
		        else // no resources. we only need to store the AppleDouble header with Finder Info
		          
		          block.data = AppleDoubleHeaderBlock
		          tarStream.write block.stringValue(false)
		          
		        end if
		        
		        // store the file name in file list
		        content.append startPath+kHeaderResourcePrefix+fileNameCleaned
		        
		        // store the position of the header in archive
		        headIndex.append headPosition
		      #endif
		      
		    else // data fork
		      
		      // open the file for reading
		      fileStream = f.OpenAsBinaryFile
		      
		      if fileStream = nil then
		        raiseError kErrorFileInvalidStream
		        return false
		      end if
		      
		      // write the file header
		      tarStream.write fileHeader
		      
		      // copy the file into the archive one block at time
		      while not fileStream.EOF
		        block.data = fileStream.read(512)
		        tarStream.write block.StringValue(false)
		      wend
		      
		      // close the file stream
		      fileStream.close
		      
		      // store the file name in file list
		      content.append startPath+fileNameCleaned
		      
		      // store the position of the header in archive
		      headIndex.append headPosition
		      
		      #if TargetMacOS
		        // check if we need to take in account Mac content (resources, finder info)
		        if not ignoreMacContent then
		          // then check if the file has a resource fork or special Finder attributes
		          if f.resourceForkLength <> 0 _
		            or f.MacType <> kNULLOSType _
		            or f.MacCreator <> kNULLOSType _
		            or f.GetFileFlagsMBS <> 0 then
		            // call again myself to handle the Mac content
		            return doAppend(f, startPath, true)
		          end if
		        end if
		      #endif
		      
		    end if
		    
		  end if
		  
		  // Well done, we have no errors!
		  return true
		  
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Extract(index as integer, targetFolder as folderItem, recursive as boolean = true) As boolean
		  
		  // extract a single file into the specified folder
		  
		  dim retValue as boolean
		  
		  // open the tar archive as binary for read
		  tarStream = file.OpenAsBinaryFile
		  
		  // check the stream is valid
		  if tarStream = nil then
		    raiseError kErrorArchiveInvalidStream
		    return false
		  end if
		  
		  // create a temporary cache for paths
		  pathsCache = new dictionary
		  
		  // invoke the extract function
		  retValue = extractFile(index, targetFolder)
		  
		  // close the archive
		  tarStream.close
		  
		  // destroy the paths cache
		  pathsCache = nil
		  
		  // check for succesfull completion
		  if retValue then
		    raiseError kErrorNoError
		  end if
		  
		  return retValue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ExtractAll(targetFolder as folderItem) As boolean
		  
		  // extract all files in archive into the given folder
		  
		  dim retValue as boolean
		  dim i, n as integer
		  
		  // open the tar archive as binary for read
		  tarStream = file.OpenAsBinaryFile
		  
		  // check the stream is valid
		  if tarStream = nil then
		    raiseError kErrorArchiveInvalidStream
		    return false
		  end if
		  
		  // create a temporary cache for paths
		  pathsCache = new dictionary
		  
		  // invoke the extract function for all files in archive
		  n = ubound(headIndex)
		  do
		    if instr(content(i), "/") = len(content(i)) or instr(content(i), "/") = 0 then
		      // Skip Mac contents since these will be extracted anyway
		      if left(nthField(content(i), "/", countfields(content(i), "/")), 2) <> "._" then
		        retValue = extractFile(i, targetFolder, true)
		      end if
		    end if
		    i = i+1
		  loop until not retValue or i > n
		  
		  // close the archive
		  tarStream.close
		  
		  // destroy the paths cache
		  pathsCache = nil
		  
		  // check for succesfull completion
		  if retValue then
		    raiseError kErrorNoError
		  end if
		  
		  return retValue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function extractFile(index as integer, targetFolder as folderItem, recursive as boolean = true) As boolean
		  
		  // recursively extract files and directories
		  
		  dim header as TARheader
		  dim tempHeader as TARheader
		  dim longHeader as TARlongHeader
		  dim longLongHeader as TARlongLongHeader
		  dim block as TARblock
		  dim i, n as integer
		  dim recurseArray() as integer
		  dim nameLen as integer
		  dim folderADentry as integer
		  dim nextSlash as integer
		  dim retValue as boolean
		  
		  dim fileName as string
		  dim longName as string
		  dim linkName as string
		  dim fileSize as integer
		  dim fileSizeBlocks as integer
		  dim fileSizeReminder as integer
		  dim fileType as TARfileType
		  dim longFileType as TARfileType
		  dim fileUID as integer
		  dim fileGID as integer
		  dim fileMode as integer
		  dim fileModDate as date
		  
		  dim p as PermissionsMBS
		  dim tempFile as folderItem
		  dim tempFolder as folderItem
		  dim tempFilename as string
		  dim tempFilePath as string
		  dim fileStream as binaryStream
		  
		  dim isMacContent as boolean
		  dim resStream as resStreamMBS
		  dim aDoubleHead as AppleDoubleHeader
		  dim aDoubleEntries() as AppleDoubleEntry
		  dim firstBlockOffset as integer
		  dim aDoubleFndrInfo as AppleDoubleFinderInfo
		  dim aDoubleExtFndrInfo as AppleDoubleExtFinderInfo
		  
		  // check the specified file is in archive
		  if index < 0 or index > ubound(headIndex) then
		    raiseError kErrorFileNotInArchive
		    return false
		  end if
		  
		  // check the target folder is valid
		  if targetFolder = nil or not targetFolder.isWriteable then
		    raiseError kErrorTargetInvalid
		    return false
		  end if
		  
		  // create the target folder if needed and check for succesfull creation
		  if not targetFolder.exists then
		    targetFolder.createAsFolder
		    if not targetFolder.exists then
		      raiseError kErrorTargetCantBeCreated
		      return false
		    end if
		  end if
		  
		  // check the archive stream is valid
		  if tarStream = nil then
		    raiseError kErrorArchiveInvalidStream
		    return false
		  end if
		  
		  // seek where the right header starts
		  tarStream.position = headIndex(index)
		  
		  // get the header
		  header.StringValue(false) = tarStream.read(512)
		  
		  // check the header validity
		  if not checkHeader(header) then
		    return false // header is not valid - bail out - checkHeader already set the error
		  end if
		  
		  // check the header type (normal or long)
		  if isLongHeader(header) then
		    
		    longName = tarStream.read(512) // read another block (long name field)
		    tempHeader.stringValue(false) = tarStream.read(512) // read another block (standard header or another link header)
		    // check the temp header validity
		    if not checkHeader(tempHeader) then
		      tarStream.close // close the file stream
		      return false // header is not valid - bail out - checkHeader already set the error
		    end if
		    
		    // check again to determine if we are handling a long-long header or a long header
		    if isLongHeader(tempHeader) then // a long-long-header
		      
		      longLongHeader.linkHeader = header // copy the header into the linkheader field of TARlongLongHeader struct
		      longLongHeader.longLinkName =  longName // set the long link name field
		      longLongHeader.longFileHeader.linkHeader = tempHeader // set the linkheader for inner TARlongHeader struct
		      longLongHeader.longFileHeader.longName = tarStream.read(512) // read another block (long name field)
		      longLongHeader.longFileHeader.regularHeader.stringValue(false) = tarStream.read(512) // read another block (standard header)
		      
		      // this is certainly a symbolic link, therefore set the file size to 0
		      fileSize = 0
		      fileSizeBlocks = 0
		      fileSizeReminder = 0
		      fileType = TARfileType(ascB(longLongHeader.longFileHeader.regularHeader.typeflag)) // get the file type
		      linkName = longLongHeader.longLinkName.defineEncoding(encodings.UTF8) // get the link name
		      fileName = longLongHeader.longFileHeader.longName.defineEncoding(encodings.UTF8) // get the file name
		      fileUID = val("&o"+longLongHeader.longFileHeader.regularHeader.uid) // get the file owner uid
		      fileGID = val("&o"+longLongHeader.longFileHeader.regularHeader.gid) // get the file owner gid
		      fileMode = val("&o"+longLongHeader.longFileHeader.regularHeader.mode) // get the file permissions
		      fileModDate = UNIXtimestampToDate(val("&o"+longLongHeader.longFileHeader.regularHeader.mtime)) // get the file modification date
		      
		    else // a long-header
		      
		      longHeader.linkHeader = header // copy the header into the linkheader field of TARlongHeader struct
		      longHeader.longName = longName // set the long name field
		      longHeader.regularHeader = tempHeader // set the regular header field
		      
		      fileSize = val("&o"+longHeader.regularHeader.fsize) // get the file size from header
		      fileSizeBlocks = floor(fileSize / 512) // store the file size in blocks
		      fileSizeReminder = fileSize - (fileSizeBlocks*512) // store the reminder size that doesn't fit a block
		      fileType = TARfileType(ascB(longHeader.regularHeader.typeflag)) // get the file type
		      longFileType = TARfileType(ascB(longHeader.linkHeader.typeflag)) // get the extended file type
		      
		      // check if the referred file is a symbolic link or not
		      if fileType = TARfileType.SymbolicLink or fileType = TARfileType.ArchivedLink then // a symlink
		        
		        // check if the longHeader refers to a long file name or to a long link name
		        if longFileType = TARfileType.LongNameSymbolicLink then // a long link name
		          
		          linkName = longHeader.longName.defineEncoding(encodings.UTF8) // get the link name
		          fileName = longHeader.regularHeader.name.defineEncoding(encodings.UTF8) // get the file name
		          
		        else // a long file name
		          
		          linkName = longHeader.regularHeader.linkname.defineEncoding(encodings.UTF8) // get the link name
		          fileName = longHeader.longName.defineEncoding(encodings.UTF8) // get the file name
		          
		        end if
		        
		      else // a regular file
		        
		        fileName = longHeader.longName.defineEncoding(encodings.UTF8) // get the file name
		        linkName = longHeader.regularHeader.linkname.defineEncoding(encodings.UTF8) // get the link name
		        
		      end if
		      
		      fileUID = val("&o"+longHeader.regularHeader.uid) // get the file owner uid
		      fileGID = val("&o"+longHeader.regularHeader.gid) // get the file owner gid
		      fileMode = val("&o"+longHeader.regularHeader.mode) // get the file permissions
		      fileModDate = UNIXtimestampToDate(val("&o"+longHeader.regularHeader.mtime)) // get the file modification date
		      
		    end if
		    
		  else // a standard header
		    
		    fileName = header.name.defineEncoding(encodings.UTF8) // get the file name
		    linkName = header.linkname.defineEncoding(encodings.UTF8) // get the link name
		    fileSize = val("&o"+header.fsize) // get the file size from header
		    fileSizeBlocks = floor(fileSize / 512) // store the file size in blocks
		    fileSizeReminder = fileSize - (fileSizeBlocks*512) // store the reminder size that doesn't fit a block
		    fileType = TARfileType(ascB(header.typeflag)) // get the file type
		    fileUID = val("&o"+header.uid) // get the file owner uid
		    fileGID = val("&o"+header.gid) // get the file owner gid
		    fileMode = val("&o"+header.mode) // get the file permissions
		    fileModDate = UNIXtimestampToDate(val("&o"+header.mtime)) // get the file modification date
		    
		  end if
		  
		  // check if the file to extract is a directory, a file, a link or something special we can't handle
		  // and behave differently
		  
		  if fileType = TARfileType.Directory then // this is a directory
		    
		    // try to get/create the directory tree
		    tempFolder = createTree(fileName, targetFolder)
		    if tempFolder = nil then
		      return false // error already handled by createTree()
		    end if
		    
		    // set the correct modification date
		    tempFolder.modificationDate = fileModDate
		    
		    // directory successfully created
		    // now check if we have to recurse sub-dirs
		    
		    if recursive then
		      
		      // create an array with subfiles to recurse
		      // only consider files
		      n = ubound(content)
		      nameLen = len(content(index))
		      
		      for i = index+1 to n
		        if i <> index then
		          if left(content(i), nameLen) = content(index) then
		            nextSlash = instr(nameLen+1, content(i), "/")
		            if nextSlash = len(content(i)) or nextSlash = 0 then
		              if left(nthField(content(i), "/", countfields(content(i), "/")), 2) <> kHeaderResourcePrefix then
		                recurseArray.append i
		              end if
		            end if
		          end if
		        end if
		      next
		      
		      n = ubound(recurseArray)
		      i = 0
		      retValue = true
		      while i <= n and retValue
		        retValue = extractFile(recurseArray(i), targetFolder, true)
		        i = i+1
		      wend
		      
		    else
		      
		      // OK. folder created and no recursion requested
		      retValue = true
		      
		    end if
		    
		    // set permissions and owner
		    #if TargetMacOS
		      tempFolder.permissions = fileMode // set permissions
		      p = tempFolder.permissionsMBS(true)
		      p.UserID = fileUID // set user ID
		      p.GroupID = fileGID // set group ID
		    #endif
		    
		    // Check if we have to  check for an associated AppleDouble file to extract
		    if ignoreMacContent then
		      return retValue // OK. folder created and no need to check for Mac content
		    end if
		    
		    // At this point, Mac content must not be ignored
		    // Check if we have an associated AppleDouble file to extract
		    
		    // extract folder name and path
		    tempFilename = nthField(fileName, "/", countfields(fileName, "/")-1)
		    tempFilePath = left(fileName, len(fileName)-len(tempFilename)+1)
		    
		    // look in content() for an AppleDouble entry with the same name
		    folderADentry = content.indexOf(tempFilePath+kHeaderResourcePrefix+tempFileName)
		    
		    if folderADentry <> -1 then
		      // AppleDouble related entry found. Try to extract it
		      retValue = extractFile(folderADentry, targetFolder, false)
		      
		    else
		      
		      return retValue // no AppleDouble file. just return the current status
		      
		    end if
		    
		    
		  elseif fileType = TARfileType.RegularFile then // this is a file
		    
		    // separate the file name from the path
		    tempFilename = nthField(fileName, "/", countfields(fileName, "/"))
		    tempFilePath = left(fileName, len(fileName)-len(tempFilename))
		    
		    // check if this file refers to an AppleDouble resource
		    if left(tempFilename, len(kHeaderResourcePrefix)) = kHeaderResourcePrefix then
		      // check if the user requested to ignore Mac content
		      if ignoreMacContent then
		        return true // just skip the file
		      else
		        isMacContent = true
		      end if
		    end if
		    
		    
		    #if TargetMacOS
		      
		      // if this is the Mac content of a file (or FOLDER) then try to write it
		      if isMacContent then
		        
		        // change the file name removing the prefix to get the real file name
		        tempFileName = mid(tempFilename, len(kHeaderResourcePrefix)+1)
		        
		        // try to get/create the directory tree
		        tempFolder = createTree(tempFilePath, targetFolder)
		        if tempFolder = nil then
		          return false // error already handled by createTree()
		        end if
		        
		        // get the new file into the right directory
		        tempFile = tempFolder.child(cleanExtractName(tempFilename))
		        if tempFile = nil then
		          raiseError kErrorTargetCantBeCreated
		          return false
		        end if
		        
		        // read the AppleDouble header
		        aDoubleHead.stringValue(false) = tarStream.read(AppleDoubleHeader.size)
		        firstBlockOffset = AppleDoubleHeader.size
		        
		        // check the AppleDouble header validity
		        if aDoubleHead.magicNumber <> &h00051607 then
		          raiseError kErrorHeaderInvalidAppleDoubleHeader
		          return false
		        end if
		        
		        // read the AppleDouble entry headers
		        for i = 0 to aDoubleHead.numberOfEntries-1
		          redim aDoubleEntries(i)
		          aDoubleEntries(i).stringValue(false) = tarStream.read(AppleDoubleEntry.size)
		          firstBlockOffset = firstBlockOffset + AppleDoubleEntry.size
		        next
		        
		        // parse the AppleDouble entries to get the corresponding data
		        for i = 0 to ubound(aDoubleEntries)
		          
		          select case aDoubleEntries(i).entryID
		            
		          case &h09 // Finder Info
		            // read the Finder Info structure
		            aDoubleFndrInfo.stringValue(false) = tarStream.read(AppleDoubleFinderInfo.size)
		            aDoubleExtFndrInfo.stringValue(false) = tarStream.read(AppleDoubleExtFinderInfo.size)
		            
		            // increase the offset in file
		            firstBlockOffset = firstBlockOffset + AppleDoubleFinderInfo.size + AppleDoubleExtFinderInfo.size
		            
		            // Set the Finder flags as from the AppleDouble header
		            if tempFile.directory then
		              call tempFile.SetFolderFlagsMBS(aDoubleFndrInfo.finderFlags)
		            else
		              tempFile.MacType = aDoubleFndrInfo.fileType
		              tempFile.MacCreator = aDoubleFndrInfo.fileCreator
		              call tempFile.SetFileFlagsMBS(aDoubleFndrInfo.finderFlags)
		            end if
		            
		            
		          case &h02 // Resource Fork
		            // test first if the declared res fork lenght > 0
		            if aDoubleEntries(i).lenght > 0 then
		              // check if the file already has a resource fork
		              if tempFile.resourceForkLength <> 0 then
		                // if requested to ignore existing files, then bail out
		                if ignoreExistingFiles then
		                  return true
		                else
		                  // if NOT requested to ignore existing files
		                  // check if we have to overwrite
		                  if not overwriteFilesOnExpanding then
		                    // overwrite not allowed - exit with an error
		                    raiseError kErrorTargetAlreadyExists
		                    return false
		                  end if
		                end if
		              end if
		              
		              // at this point, either the file doesn't have resourceFork or
		              // needs to be overwritten or it's a directory (no res fork for directories)
		              
		              if not tempFile.directory then
		                // open or create a resource stream for the new file
		                if tempFile.exists then
		                  resStream = tempFile.OpenAsResStreamMBS(true)
		                else
		                  resStream = tempFile.CreateResStreamMBS("", "")
		                end if
		                if resStream = nil then
		                  raiseError kErrorFileInvalidResourceStream
		                  return false
		                end if
		                
		                
		                // read the remaining part of a block that is the first chunk of resource fork
		                // and write it to the resource stream
		                if fileSize <= 512 then
		                  resStream.write tarStream.read(fileSizeReminder - firstBlockOffset)
		                else
		                  resStream.write tarStream.read(512-firstBlockOffset)
		                  // copy all blocks from the archive to the file resource fork
		                  // starting from 2nd block since the first was already read
		                  for i = 2 to fileSizeBlocks
		                    block.StringValue(false) = tarStream.read(512)
		                    resStream.write block.StringValue(false)
		                  next
		                  resStream.write tarStream.read(fileSizeReminder)
		                end if
		                
		                // close the stream for the file resource fork
		                resStream.close
		                
		              else // this is a directory and the AppleDouble header says a resourceFork is available (bad archive?)
		                
		                // we just ignore the bad AppleDouble header and simply skip the resourceFork data
		                if fileSize <= 512 then
		                  call tarStream.read(fileSizeReminder - firstBlockOffset) // to /dev/null
		                else
		                  call tarStream.read(512-firstBlockOffset)
		                  // read all blocks from the archive and send them to a black hole
		                  // starting from 2nd block since the first was already read
		                  for i = 2 to fileSizeBlocks
		                    call tarStream.read(512)
		                  next
		                  call tarStream.read(fileSizeReminder)
		                end if
		                
		              end if
		              
		            end if
		            
		          end select
		          
		        next
		        
		        return true // OK. Mac content written
		        
		      end if
		      
		    #endif
		    
		    // Here we have a standard file or we are not on Mac and
		    // therefore an AppleDouble file will be written as a standard file
		    // with the kHeaderResourcePrefix prepending the name
		    
		    // try to get/create the directory tree
		    tempFolder = createTree(tempFilePath, targetFolder)
		    if tempFolder = nil then
		      return false // error already handled by createTree()
		    end if
		    
		    // create the new file into the right directory
		    tempFile = tempFolder.child(cleanExtractName(tempFilename))
		    if tempFile = nil then
		      raiseError kErrorTargetCantBeCreated
		      return false
		    end if
		    
		    // check if the file already exists
		    if tempFile.length <> 0 then // if tempFile.exists then
		      // if requested to ignore existing files, then bail out
		      if ignoreExistingFiles then
		        return true
		      else
		        // if NOT requested to ignore existing files
		        // check if we have to overwrite
		        if not overwriteFilesOnExpanding then
		          // overwrite not allowed - exit with an error
		          raiseError kErrorTargetAlreadyExists
		          return false
		        end if
		      end if
		    end if
		    
		    // at this point, either the file doesn't exists or
		    // needs to be overwritten
		    
		    // create a stream for the new file
		    #if TargetMacOS
		      if tempFile.ResourceForkLength = 0 then
		        fileStream = tempFile.CreateBinaryFile("")
		      else
		        fileStream = tempFile.OpenAsBinaryFile(true)
		      end if
		    #else
		      fileStream = tempFile.CreateBinaryFile("")
		    #endif
		    if fileStream = nil then
		      raiseError kErrorTargetInvalidStream
		    end if
		    
		    // copy all blocks from the archive to the new file
		    for i = 1 to fileSizeBlocks
		      block.StringValue(false) = tarStream.read(512)
		      fileStream.write block.StringValue(false)
		    next
		    fileStream.write tarStream.read(fileSizeReminder)
		    
		    // close the stream for the new file
		    fileStream.close
		    
		    // set the right modification date
		    tempFile.modificationDate = fileModDate
		    
		    #if TargetMacOS
		      tempFile.permissions = fileMode // set permissions
		      p = tempFile.permissionsMBS(true)
		      p.UserID = fileUID // set userID
		      p.GroupID = fileGID // set groupID
		    #endif
		    
		    // if this is already Mac content, just return
		    if isMacContent then
		      return true
		    end if
		    
		    // check if we have to  check for an associated AppleDouble file to extract
		    if ignoreMacContent then
		      return true // OK. file created and no need to check for Mac content
		    end if
		    
		    // At this point, Mac content must not be ignored
		    // Check if we have an associated AppleDouble file to extract
		    
		    // check the next record
		    if (index < ubound(content) and content(index+1) = tempFilePath+kHeaderResourcePrefix+tempFilename) then
		      // we have the AppleDouble file for this file to extract
		      retValue = extractFile(index+1, targetFolder, false)
		      
		      // check the previous record
		    elseif (index > 0 and content(index-1) = tempFilePath+kHeaderResourcePrefix+tempFilename) then
		      // we have the AppleDouble file for this file to extract
		      retValue = extractFile(index-1, targetFolder, false)
		      
		      // no associated AppleDouble file. just return true
		    else
		      retValue = true
		      
		    end if
		    
		    return retValue // return
		    
		  elseif fileType = TARfileType.SymbolicLink or fileType = TARfileType.ArchivedLink then
		    
		    // handle symlinks
		    // separate the file name from the path
		    tempFilename = nthField(fileName, "/", countfields(fileName, "/"))
		    tempFilePath = left(fileName, len(fileName)-len(tempFilename))
		    
		    // try to get/create the directory tree
		    tempFolder = createTree(tempFilePath, targetFolder)
		    if tempFolder = nil then
		      return false // error already handled by createTree()
		    end if
		    
		    // create the new file into the right directory
		    tempFile = tempFolder.child(cleanExtractName(tempFilename))
		    if tempFile = nil then
		      raiseError kErrorTargetCantBeCreated
		      return false
		    end if
		    
		    // create the symbolic link
		    if not setlink(linkName, tempFile.NativePath) then
		      raiseError kErrorTargetLinkCantBeCreated
		      return false
		    end if
		    
		    return true  // OK. symbolic link written
		    
		  else // we have a special file. Sorry we can't handle this
		    
		    // ignore the special file if requested or bail out with an error
		    if not ignoreSpecialFiles then
		      raiseError kErrorFileNotSupported
		      return false
		    end if
		    
		    return true // just skip the file
		    
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function FileCount() As integer
		  
		  // return the number of files in archive
		  
		  return ubound(content)+1
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function FileList() As string()
		  
		  // return the array with the list of file's names in archive
		  
		  return content
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function getContent() As boolean
		  
		  // parse the archive and store an index of headers as well a list of files
		  
		  dim header as TARheader
		  dim longHeader as TARlongHeader
		  dim longlongHeader as TARlongLongHeader
		  dim tempHeader as TARheader
		  dim longName as string
		  dim fsize as integer
		  dim reminder as integer
		  dim headPosition as integer
		  
		  // open the file as binaryStream
		  tarStream = file.OpenAsBinaryFile
		  
		  // check the file stream is valid
		  if tarStream = nil then
		    raiseError kErrorArchiveInvalidStream // can't get the file stream
		    return false
		  end if
		  
		  // get the first header and ...
		  header.StringValue(false) = tarStream.read(512)
		  
		  do // ... start looping until the EOF is reached
		    
		    // check the header validity
		    if not checkHeader(header) then
		      tarStream.close // close the file stream
		      return false // header is not valid - bail out - checkHeader already set the error
		    end if
		    
		    // check the header type (normal or long)
		    if isLongHeader(header) then
		      
		      longName = tarStream.read(512) // read another block (long name field)
		      tempHeader.stringValue(false) = tarStream.read(512) // read another block (standard header or another link header)
		      // check the temp header validity
		      if not checkHeader(tempHeader) then
		        tarStream.close // close the file stream
		        return false // header is not valid - bail out - checkHeader already set the error
		      end if
		      
		      // check again to determine if we are handling a long-long header or a long header
		      if isLongHeader(tempHeader) then // a long-long-header
		        
		        longLongHeader.linkHeader = header // copy the header into the linkheader field of TARlongLongHeader struct
		        longLongHeader.longLinkName =  longName // set the long link name field
		        longLongHeader.longFileHeader.linkHeader = tempHeader // set the linkheader for inner TARlongHeader struct
		        longLongHeader.longFileHeader.longName = tarStream.read(512) // read another block (long name field)
		        longLongHeader.longFileHeader.regularHeader.stringValue(false) = tarStream.read(512) // read another block (standard header)
		        
		        // a long-long-header is certainly leading to a symbolic link
		        content.append longLongHeader.longFileHeader.longName.defineEncoding(encodings.UTF8) // store the link name in array
		        
		        // don't get the file size from header - it doesn't matter for symlinks
		        fsize = 0
		        
		      else // a long-header
		        
		        longHeader.linkHeader = header // copy the header into the linkheader field of TARlongHeader struct
		        longHeader.longName = longName // set the long name field
		        longHeader.regularHeader = tempHeader // set the regular header field
		        
		        // behave differently for links and regular files
		        if ascB(longHeader.linkHeader.typeflag) = int32(TARfileType.LongNameSymbolicLink) then
		          content.append longHeader.regularHeader.name.defineEncoding(encodings.UTF8) // store the link name in array
		        else
		          content.append longHeader.longName.defineEncoding(encodings.UTF8) // store the file name in array
		        end if
		        fsize = val("&o"+longHeader.regularHeader.fsize) // get the file size from header
		        
		      end if
		      
		    else // a standard header
		      
		      content.append header.name.defineEncoding(encodings.UTF8) // store the file name in array
		      fsize = val("&o"+header.fsize) // get the file size from header
		      
		    end if
		    
		    headIndex.append headPosition // store the position in file for this header
		    
		    tarStream.position = tarStream.position + fsize // advance in file to skip n bytes (n = filesize)
		    
		    // since TAR always write 512 bytes blocks, calculate the padding bytes and skip them
		    reminder = fsize mod 512
		    if reminder > 0 then
		      tarStream.position = tarStream.position + (512-reminder)
		    end if
		    headPosition = tarStream.position
		    
		    // read another header
		    header.StringValue(false) = tarStream.read(512)
		    
		    // check if the header is full of NILs (marking the end of archive)
		    if header.name = "" then
		      // we reached the end - set position to EOF to exit the loop
		      tarStream.position = tarStream.length
		    end if
		    
		  loop until tarStream.EOF
		  
		  tarStream.close // close the file stream
		  
		  'dim tempSt as string = nodeTree.getList
		  'dim faile as folderItem = getSaveFolderItem("", "")
		  'dim failestream as textOutputStream
		  'if faile <> nil then
		  'failestream = faile.CreateTextFile
		  'failestream.write tempSt
		  'failestream.flush
		  'failestream.close
		  'end if
		  
		  //  caller already handle the noError
		  return true
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function getlink(f as folderItem) As string
		  
		  dim returnPath as string
		  
		  #if TargetMacOS
		    dim b as memoryBlock
		    dim size as integer
		    dim pathLen as integer
		    
		    declare function readlink lib "System" (path as cstring, buf as ptr, bufsize as integer) as integer
		    
		    b = newMemoryBlock(512)
		    
		    pathLen = readlink(f.NativePath, b, b.size)
		    
		    if  pathLen <> -1 then
		      returnPath = b.stringValue(0, pathLen)
		    end if
		    
		  #endif
		  
		  return returnPath
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function isLongHeader(header as TARheader) As boolean
		  
		  // check if the passed header refers to a long header
		  
		  if header.typeflag = chrB(int32(TARfileType.LongNameSymbolicLink)) or _
		    header.typeflag = chrB(int32(TARfileType.LongNameRegularFile)) then
		    return true
		  else
		    return false
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function LastError() As integer
		  
		  // return the last error status
		  
		  return myError
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function makeADHeader(f as folderItem) As string
		  
		  // create AppleDouble header for the passed file
		  // handle Finder Info and Resource Fork
		  
		  dim fIleFlags as integer
		  dim fileHasResourceFork as boolean
		  dim aDoubleHead as AppleDoubleHeader
		  dim aDoubleFInfoEntry as AppleDoubleEntry
		  dim aDoubleRsrcEntry as AppleDoubleEntry
		  dim aDoubleFndrInfo as AppleDoubleFinderInfo
		  dim aDoubleExtFndrInfo as AppleDoubleExtFinderInfo
		  dim aDoubleOffset as integer
		  dim AppleDoubleHeaderBlock as string
		  
		  // get Mac content info
		  if f.Directory then
		    FileFlags = f.GetFolderFlagsMBS
		  else
		    FileFlags = f.GetFileFlagsMBS
		  end if
		  fileHasResourceFork = (f.resourceForkLength <> 0)
		  
		  // build the AppleDoubleHeader
		  aDoubleHead.magicNumber = &h00051607
		  aDoubleHead.versionNumber = &h00020000
		  aDoubleHead.fileSystem = "Mac OS X"
		  aDoubleHead.numberOfEntries = 0 // temp value. next we'll update this
		  
		  aDoubleOffset = aDoubleHead.size // starting offset for AppleDouble entries
		  
		  // check if we need to save Finder Info
		  if FileFlags <> 0 then
		    // increase the offset and number of entries in AppleDouble header
		    aDoubleHead.numberOfEntries = aDoubleHead.numberOfEntries + 1
		    aDoubleOffset = aDoubleOffset + AppleDoubleEntry.size
		  end if
		  
		  // check if we need to save resource fork
		  if fileHasResourceFork then
		    // increase the offset and number of entries in AppleDouble file
		    aDoubleHead.numberOfEntries = aDoubleHead.numberOfEntries + 1
		    aDoubleOffset = aDoubleOffset + AppleDoubleEntry.size
		  end if
		  
		  // start putting the header in a string
		  AppleDoubleHeaderBlock = aDoubleHead.stringValue(false)
		  
		  if FileFlags <> 0 then
		    // build the entry header for FinderInfo
		    aDoubleFInfoEntry.entryID = &h09
		    aDoubleFInfoEntry.offset = aDoubleOffset
		    aDoubleFInfoEntry.lenght = aDoubleFndrInfo.size + aDoubleExtFndrInfo.size
		    aDoubleOffset = aDoubleOffset + aDoubleFInfoEntry.lenght // advance the offset to the next entry
		    
		    // continue building the string for the header
		    AppleDoubleHeaderBlock = AppleDoubleHeaderBlock + aDoubleFinfoEntry.stringValue(false)
		    
		    // build the FinderInfo struct
		    aDoubleFndrInfo.fileType = f.MacType
		    aDoubleFndrInfo.fileCreator = f.MacCreator
		    aDoubleFndrInfo.finderFlags = FileFlags
		    aDoubleFndrInfo.point = 0
		    aDoubleFndrInfo.reserved = 0
		    
		    // build the Extended FinderInfo struct
		    aDoubleExtFndrInfo.reserved1 = 0
		    aDoubleExtFndrInfo.extendedFinderFlags = 0
		    aDoubleExtFndrInfo.reserved2 = 0
		    aDoubleExtFndrInfo.putAwayFolderID = 0
		  end if
		  
		  // check if we need to save resource fork
		  if fileHasResourceFork then
		    // build the entry header for Resource Fork
		    aDoubleRsrcEntry.entryID = &h02
		    aDoubleRsrcEntry.offset = aDoubleOffset
		    aDoubleRsrcEntry.lenght = f.resourceForkLength
		    aDoubleOffset = aDoubleOffset + aDoubleRsrcEntry.lenght // advance the offset to the next entry (future?)
		    
		    // continue building the string for the header
		    AppleDoubleHeaderBlock = AppleDoubleHeaderBlock + aDoubleRsrcEntry.stringValue(false)
		    
		  end if
		  
		  // Now add the file info data to the header string
		  if FileFlags <> 0 then
		    AppleDoubleHeaderBlock = AppleDoubleHeaderBlock _
		    + aDoubleFndrInfo.stringValue(false) _
		    + aDoubleExtFndrInfo.stringValue(false)
		  end if
		  
		  // return the string
		  return AppleDoubleHeaderBlock
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function makeTARHeader(f as folderItem, startPath as string, isAppleDoubleHeader as boolean = false) As string
		  
		  // create a TAR header for the passed file
		  
		  dim header as TARheader
		  dim longHeader as TARlongHeader
		  dim longLongHeader as TARlongLongHeader
		  dim isLongLongHeader as boolean
		  dim fileType as TARfileType
		  dim tempFile as folderItem
		  dim linkPath as string
		  dim fileName as string
		  dim linkName as string
		  dim AppleDoubleSize as integer
		  dim d as DarwinChmodMBS
		  dim p as PermissionsMBS
		  
		  // check if the file is valid
		  if f = nil or not f.exists then
		    raiseError kErrorFileInvalid
		    return ""
		  end if
		  
		  // check if the file is readable
		  if not file.isReadable then
		    raiseError kErrorFileNotReadable
		    return ""
		  end if
		  
		  // prepend the startPath and append trailing slash to directories
		  if isAppleDoubleHeader then
		    fileName = startPath+kHeaderResourcePrefix+cleanArchiveName(f.name)
		  else
		    fileName = startPath+cleanArchiveName(f.name)
		    if f.Directory then
		      fileName = fileName+"/"
		    end if
		  end if
		  
		  
		  #if TargetMacOS
		    
		    // get Darwin attrubutes
		    d = new DarwinChmodMBS
		    if d = nil or not (d.lstat(f.NativePath) = 0) then
		      
		      d = nil
		      // get at least permissions if lstat() didn't work
		      p = f.PermissionsMBS(true)
		      
		      if isAppleDoubleHeader then
		        fileType = TARfileType.RegularFile
		      else
		        if f.Directory then
		          fileType = TARfileType.Directory
		        else
		          fileType = TARfileType.RegularFile
		        end if
		      end if
		      
		    else
		      
		      if isAppleDoubleHeader then
		        
		        fileType = TARfileType.RegularFile
		      else
		        
		        // get BSD filetype
		        select case bitwise.BitAnd(d.mode, &o0170000)
		          
		        case &o0010000
		          fileType = TARfileType.FIFOSpecialFile
		          // file not supported
		          raiseError kErrorFileNotSupported
		          return ""
		          
		        case &o0020000
		          fileType = TARfileType.CharSpecialDevice
		          // file not supported
		          raiseError kErrorFileNotSupported
		          return ""
		          
		        case &o0040000
		          fileType = TARfileType.Directory
		          
		        case &o0060000
		          fileType = TARfileType.BlockSpecialDevice
		          // file not supported
		          raiseError kErrorFileNotSupported
		          return ""
		          
		        case &o0100000
		          fileType = TARfileType.RegularFile
		          
		        case &o0120000
		          fileType = TARfileType.SymbolicLink
		          
		        case &o0140000
		          // if the file is a socket return empty header
		          raiseError kErrorFileNotSupported
		          return ""
		          
		        case &o0160000
		          // if the file is a whiteout return empty header
		          raiseError kErrorFileNotSupported
		          return ""
		          
		        end select
		        
		      end if
		      
		    end if
		    
		  #else
		    
		    if f.Directory then
		      fileType = TARfileType.Directory
		    else
		      fileType = TARfileType.RegularFile
		    end if
		    
		  #endif
		  
		  
		  // Start composing the header, assuming this is a regular header.
		  // later we check for the name size and, if exceeding 100 bytes, we will
		  // create a long header and this regular header will become part of it
		  
		  // file name
		  header.name = fileName
		  
		  // set permissions. On Windows set the kHeaderDefaultMode
		  #if TargetMacOS
		    header.mode = right(kMaskEight + oct(f.permissions), 7)
		  #else
		    header.mode = kHeaderDefaultMode
		  #endif
		  
		  // set the owner UID
		  #if TargetMacOS
		    if d <> nil then
		      header.uid = right(kMaskEight + oct(d.uid), 7)
		    elseif p <> nil then
		      header.uid = right(kMaskEight + oct(p.UserID), 7)
		    else
		      header.uid = kHeaderDefaultUserGroup // user 501 is the generally the first user
		    end if
		  #else
		    header.uid = kHeaderDefaultUserGroup // user 501 is the generally the first user
		  #endif
		  
		  // set the group GID
		  #if TargetMacOS
		    if d <> nil then
		      header.gid = right(kMaskEight + oct(d.gid), 7)
		    elseif p <> nil then
		      header.gid = right(kMaskEight + oct(p.GroupID), 7)
		    else
		      header.gid = kHeaderDefaultUserGroup // group 501 is the generally the first group
		    end if
		  #else
		    header.gid = kHeaderDefaultUserGroup // group 501 is the generally the first group
		  #endif
		  
		  // set the file size in bytes (0 for a link) - in octal
		  if fileType = TARfileType.ArchivedLink or fileType = TARfileType.SymbolicLink then
		    header.fsize = kMaskTwelve
		  else
		    if isAppleDoubleHeader then
		      AppleDoubleSize = AppleDoubleHeader.size
		      if f.resourceForkLength <> 0 then
		        AppleDoubleSize = AppleDoubleSize + f.resourceForkLength + AppleDoubleEntry.size
		      end if
		      if f.Directory then
		        if f.GetFolderFlagsMBS <> 0 then
		          AppleDoubleSize = AppleDoubleSize + AppleDoubleEntry.size + AppleDoubleFinderInfo.size + AppleDoubleExtFinderInfo.size
		        end if
		      else
		        if f.GetFileFlagsMBS <> 0 then
		          AppleDoubleSize = AppleDoubleSize + AppleDoubleEntry.size + AppleDoubleFinderInfo.size + AppleDoubleExtFinderInfo.size
		        end if
		      end if
		      header.fsize = right(kMaskTwelve+oct(AppleDoubleSize), 11)
		    else
		      header.fsize = right(kMaskTwelve+oct(f.length), 11)
		    end if
		  end if
		  
		  // set the modify time in UNIX timestamp format
		  header.mtime = right(kMaskTwelve+oct(DateToUNIXtimestamp(f.modificationDate)), 11)
		  
		  // set file type
		  header.typeflag = chrB(int32(fileType))
		  
		  // set the link name if needed
		  #if TargetMacOS
		    if fileType = TARfileType.ArchivedLink or fileType = TARfileType.SymbolicLink then
		      
		      linkName = getLink(f)
		      
		      // try to get the full path if getLink didn't work
		      if linkName = "" then
		        // get the referred file
		        tempFile = getTrueFolderItem(f.NativePath, Folderitem.PathTypeNative)
		        
		        // store the absolute (native) path
		        if tempFile <> nil then
		          linkName = tempFile.NativePath
		        else
		          raiseError kErrorFileCantGetReferredLink
		          return ""
		        end if
		      end if
		      
		      header.linkname = linkName
		    end if
		  #endif
		  
		  // set magic identifier for USTAR
		  header.magic = kHeaderMagic
		  
		  // set USTAR version
		  header.version = kHeaderVersion
		  
		  // set user name
		  header.uname = f.owner
		  
		  // set group name
		  header.gname = f.group
		  
		  // set device major
		  header.devmajor = kMaskEight
		  
		  // set device minor
		  header.devminor = kMaskEight
		  
		  // set prefix
		  // leave null
		  
		  // calculate and set checksum
		  header.checksum = "        "
		  header.checksum = right(kMaskEight+oct(CheckSum(header.stringValue(false))), 7)
		  
		  
		  // check if this file require a longHeader to accomodate a long name or long link name
		  if lenb(fileName) <= 100 and lenb(linkName) <= 100 then
		    
		    // both file and link names are shorter than or equal 100
		    // no longHeader needed - just return the regular header
		    return header.StringValue(false)
		    
		  elseif lenb(fileName) > 100 and lenb(linkName) > 100 then
		    
		    // both file and link names are langer than 100
		    // a long-long header required
		    isLongLongHeader = true
		    
		  else
		    
		    // only file or link name is longer than 100
		    // a long header is needed
		    // just proceed without setting isLongLongHeader
		    
		  end if
		  
		  
		  // At this point we need a long header or a long-long header
		  
		  if isLongLongHeader then // a long-long header
		    
		    // setup the linkHeader
		    
		    // set the symbolic name
		    longLongHeader.linkHeader.name = kHeaderLongLink
		    
		    // set permissions
		    longLongHeader.linkHeader.mode = header.mode
		    
		    // set owner UID
		    longLongHeader.linkHeader.uid = header.uid
		    
		    // set group GID
		    longLongHeader.linkHeader.gid = header.gid
		    
		    // set file size in bytes - for long-long header it's the length of (link + null terminator) in octal
		    longLongHeader.linkHeader.fsize = right(kMaskTwelve+oct(lenb(linkName)+1), 11)
		    
		    // set modify time
		    longLongHeader.linkHeader.mtime = header.mtime
		    
		    // set file type
		    longLongHeader.linkHeader.typeflag = chrB(int32(TARfileType.LongNameSymbolicLink))
		    
		    // set link name
		    // leave null
		    
		    // set magic identifier for USTAR
		    longLongHeader.linkHeader.magic = kHeaderMagic
		    
		    // set USTAR version
		    longLongHeader.linkHeader.version = kHeaderVersion
		    
		    // set user name
		    // leave null
		    
		    // set group name
		    // leave null
		    
		    // set device major
		    longLongHeader.linkHeader.devmajor = kMaskEight
		    
		    // set device minor
		    longLongHeader.linkHeader.devminor = kMaskEight
		    
		    // set prefix
		    // leave null
		    
		    // calculate and set checksum
		    longLongHeader.linkHeader.checksum = "        "
		    longLongHeader.linkHeader.checksum = right(kMaskEight+oct(CheckSum(longLongHeader.linkHeader.stringValue(false))), 7)
		    
		    //////////////////////////////////////////////////////////////////////
		    
		    // set long link name
		    longLongHeader.longLinkName = linkName
		    
		    //////////////////////////////////////////////////////////////////////
		    
		    // setup the longFileHeader->linkHeader
		    
		    // set the symbolic name
		    longLongHeader.longFileHeader.linkHeader.name = kHeaderLongLink
		    
		    // set permissions
		    longLongHeader.longFileHeader.linkHeader.mode = header.mode
		    
		    // set owner UID
		    longLongHeader.longFileHeader.linkHeader.uid = header.uid
		    
		    // set group GID
		    longLongHeader.longFileHeader.linkHeader.gid = header.gid
		    
		    // set file size in bytes - for long header it's the length of (name + null terminator) in octal
		    longLongHeader.longFileHeader.linkHeader.fsize = right(kMaskTwelve+oct(lenb(fileName)+1), 11)
		    
		    // set modify time
		    longLongHeader.longFileHeader.linkHeader.mtime = header.mtime
		    
		    // set file type
		    longLongHeader.longFileHeader.linkHeader.typeflag = chrB(int32(TARfileType.LongNameRegularFile))
		    
		    // set link name
		    // leave null
		    
		    // set magic identifier for USTAR
		    longLongHeader.longFileHeader.linkHeader.magic = kHeaderMagic
		    
		    // set USTAR version
		    longLongHeader.longFileHeader.linkHeader.version = kHeaderVersion
		    
		    // set user name
		    // leave null
		    
		    // set group name
		    // leave null
		    
		    // set device major
		    longLongHeader.longFileHeader.linkHeader.devmajor = kMaskEight
		    
		    // set device minor
		    longLongHeader.longFileHeader.linkHeader.devminor = kMaskEight
		    
		    // set prefix
		    // leave null
		    
		    // calculate and set checksum
		    longLongHeader.longFileHeader.linkHeader.checksum = "        "
		    longLongHeader.longFileHeader.linkHeader.checksum = _
		    right(kMaskEight+oct(CheckSum(longLongHeader.longFileHeader.linkHeader.stringValue(false))), 7)
		    
		    //////////////////////////////////////////////////////////////////////
		    
		    // set the longFileHeader->long name
		    longLongHeader.longFileHeader.longName = fileName
		    
		    //////////////////////////////////////////////////////////////////////
		    
		    // set the longFileHeader->regular header
		    longLongHeader.longFileHeader.regularHeader = header
		    
		    ///////////////////////////////////////////////////////////////////
		    
		    // return the longlongHeader
		    return longLongHeader.StringValue(false)
		    
		    
		  else // a long header
		    
		    
		    // set the symbolic name
		    longHeader.linkHeader.name = kHeaderLongLink
		    
		    // set permissions
		    longHeader.linkHeader.mode = header.mode
		    
		    // set owner UID
		    longHeader.linkHeader.uid = header.uid
		    
		    // set group GID
		    longHeader.linkHeader.gid = header.gid
		    
		    // set file size in bytes - for long header it's the length of (name + null terminator) in octal
		    longHeader.linkHeader.fsize = right(kMaskTwelve+oct(lenb(fileName)+1), 11)
		    
		    // set modify time
		    longHeader.linkHeader.mtime = header.mtime
		    
		    // set file type
		    if fileType = TARfileType.ArchivedLink or fileType = TARfileType.SymbolicLink then
		      longHeader.linkHeader.typeflag = chrB(int32(TARfileType.LongNameSymbolicLink))
		    else
		      longHeader.linkHeader.typeflag = chrB(int32(TARfileType.LongNameRegularFile))
		    end if
		    
		    // set link name
		    // leave null
		    
		    // set magic identifier for USTAR
		    longHeader.linkHeader.magic = kHeaderMagic
		    
		    // set USTAR version
		    longHeader.linkHeader.version = kHeaderVersion
		    
		    // set user name
		    // leave null
		    
		    // set group name
		    // leave null
		    
		    // set device major
		    longHeader.linkHeader.devmajor = kMaskEight
		    
		    // set device minor
		    longHeader.linkHeader.devminor = kMaskEight
		    
		    // set prefix
		    // leave null
		    
		    // calculate and set checksum
		    longHeader.linkHeader.checksum = "        "
		    longHeader.linkHeader.checksum = right(kMaskEight+oct(CheckSum(longHeader.linkHeader.stringValue(false))), 7)
		    
		    ///////////////////////////////////////////////////////////////////
		    
		    // set long name
		    if fileType = TARfileType.ArchivedLink or fileType = TARfileType.SymbolicLink then
		      longHeader.longName = linkName
		    else
		      longHeader.longName = fileName
		    end if
		    
		    ///////////////////////////////////////////////////////////////////
		    
		    // set regular header
		    longHeader.regularHeader = header
		    
		    ///////////////////////////////////////////////////////////////////
		    
		    // return the longHeader
		    return longHeader.StringValue(false)
		    
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Open() As boolean
		  
		  // open the TAR archive an retrieve the list of files as well
		  // the index of headers
		  
		  // check the file is not NIL
		  if file = nil then
		    raiseError kErrorArchiveInvalid // file is NIL
		    return false
		  end if
		  
		  if file.exists then
		    // if file exists check if it's readable
		    if file.isReadable then
		      // get the content of the file
		      if self.getContent then
		        raiseError kErrorNoError // no errors
		        return true
		      else
		        // getContent already handled the error
		        return false
		      end if
		    else
		      raiseError kErrorArchiveNotReadable // file not readable
		      return false
		    end if
		    
		  else
		    // file doesn't exists yet, return true
		    raiseError kErrorNoError // no errors
		    return true
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub raiseError(errorCode as integer)
		  
		  // set the last error in operations and invoke the Error() event
		  
		  myError = errorCode
		  if myError <> kErrorNoError then
		    Error myError
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function setlink(linkedPath as string, linkPath as string) As boolean
		  
		  #if TargetMacOS
		    dim err as integer
		    
		    declare function symlink lib "System" (name1 as cstring, name2 as cstring) as integer
		    
		    err = symlink(linkedPath, linkPath)
		    
		    if err = 0 then
		      return true
		    else
		      return false
		    end if
		    
		  #else
		    
		    return false
		    
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function UNIXtimestampToDate(ts as double) As date
		  dim diffDate as new date
		  dim retDate as new date
		  
		  diffDate.day = 1
		  diffDate.month = 1
		  diffDate.year = 1970
		  diffDate.hour = 0 + diffDate.gMTOffset
		  diffDate.minute = 0
		  diffDate.second = 0
		  
		  retDate.totalSeconds = ts + diffDate.totalSeconds
		  
		  return retDate
		End Function
	#tag EndMethod


	#tag Hook, Flags = &h0
		Event Error(errorCode as integer)
	#tag EndHook


	#tag Note, Name = About
		
		TARarchive version 1.0.2
		
		©2007 Massimo Valle
		
	#tag EndNote


	#tag Property, Flags = &h21
		Private content() As string
	#tag EndProperty

	#tag Property, Flags = &h21
		Private file As folderItem
	#tag EndProperty

	#tag Property, Flags = &h21
		Private headIndex() As integer
	#tag EndProperty

	#tag Property, Flags = &h0
		ignoreChecksumErrors As boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		ignoreExistingFiles As boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		ignoreMacContent As boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		ignoreSpecialFiles As boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private kNULLOSType As string
	#tag EndProperty

	#tag Property, Flags = &h21
		Private myError As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		overwriteFilesOnExpanding As boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private pathsCache As dictionary
	#tag EndProperty

	#tag Property, Flags = &h21
		Private tarStream As binaryStream
	#tag EndProperty


	#tag Constant, Name = kErrorArchiveInvalid, Type = Double, Dynamic = False, Default = \"-100", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorArchiveInvalidStream, Type = Double, Dynamic = False, Default = \"-102", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorArchiveNotReadable, Type = Double, Dynamic = False, Default = \"-101", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorFileCantGetReferredLink, Type = Double, Dynamic = False, Default = \"-206", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorFileInvalid, Type = Double, Dynamic = False, Default = \"-200", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorFileInvalidResourceStream, Type = Double, Dynamic = False, Default = \"-205", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorFileInvalidStream, Type = Double, Dynamic = False, Default = \"-202", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorFileNotInArchive, Type = Double, Dynamic = False, Default = \"-204", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorFileNotReadable, Type = Double, Dynamic = False, Default = \"-201", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorFileNotSupported, Type = Double, Dynamic = False, Default = \"-203", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorHeaderChecksumBad, Type = Double, Dynamic = False, Default = \"-301", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorHeaderInvalid, Type = Double, Dynamic = False, Default = \"-300", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorHeaderInvalidAppleDoubleHeader, Type = Double, Dynamic = False, Default = \"-302", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorNoError, Type = Double, Dynamic = False, Default = \"0", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorTargetAlreadyExists, Type = Double, Dynamic = False, Default = \"-403", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorTargetCantBeCreated, Type = Double, Dynamic = False, Default = \"-401", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorTargetInvalid, Type = Double, Dynamic = False, Default = \"-400", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorTargetInvalidStream, Type = Double, Dynamic = False, Default = \"-402", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kErrorTargetLinkCantBeCreated, Type = Double, Dynamic = False, Default = \"-404", Scope = Public
	#tag EndConstant

	#tag Constant, Name = kHeaderDefaultMode, Type = String, Dynamic = False, Default = \"0000755", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kHeaderDefaultUserGroup, Type = String, Dynamic = False, Default = \"0000765", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kHeaderLongLink, Type = String, Dynamic = False, Default = \"@LongLink", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kHeaderMagic, Type = String, Dynamic = False, Default = \"ustar", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kHeaderResourcePrefix, Type = String, Dynamic = False, Default = \"._", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kHeaderVersion, Type = String, Dynamic = False, Default = \"00", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kMaskEight, Type = String, Dynamic = False, Default = \"0000000", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kMaskTwelve, Type = String, Dynamic = False, Default = \"00000000000", Scope = Private
	#tag EndConstant


	#tag Structure, Name = AppleDoubleEntry, Flags = &h0
		entryID as UInt32
		  offset as UInt32
		lenght as UInt32
	#tag EndStructure

	#tag Structure, Name = AppleDoubleExtFinderInfo, Flags = &h0
		reserved1 as UInt64
		  extendedFinderFlags as UInt16
		  reserved2 as Int16
		putAwayFolderID as Int32
	#tag EndStructure

	#tag Structure, Name = AppleDoubleFinderInfo, Flags = &h0
		fileType as OSType
		  fileCreator as OSType
		  finderFlags as UInt16
		  point as UInt32
		reserved as UInt16
	#tag EndStructure

	#tag Structure, Name = AppleDoubleHeader, Flags = &h0
		magicNumber as UInt32
		  versionNumber as UInt32
		  fileSystem as string*16
		numberOfEntries as UInt16
	#tag EndStructure

	#tag Structure, Name = TARblock, Flags = &h0
		data as string*512
	#tag EndStructure

	#tag Structure, Name = TARheader, Flags = &h0
		name as string*100
		  mode as string*8
		  uid as string*8
		  gid as string*8
		  fsize as string*12
		  mtime as string*12
		  checksum as string*8
		  typeflag as string*1
		  linkname as string*100
		  magic as string*6
		  version as string*2
		  uname as string*32
		  gname as string*32
		  devmajor as string*8
		  devminor as string*8
		  prefix as string*155
		padding as string*12
	#tag EndStructure

	#tag Structure, Name = TARlongHeader, Flags = &h0
		linkHeader as TARheader
		  longName as string*512
		regularHeader as TARheader
	#tag EndStructure

	#tag Structure, Name = TARlongLongHeader, Flags = &h0
		linkHeader as TARheader
		  longLinkName as string*512
		longFileHeader as TARlongHeader
	#tag EndStructure


	#tag Enum, Name = TARfileType, Flags = &h0
		RegularFile = 48
		  ArchivedLink = 49
		  SymbolicLink = 50
		  CharSpecialDevice = 51
		  BlockSpecialDevice = 52
		  Directory = 53
		  FIFOSpecialFile = 54
		  Reserved = 55
		  LongNameSymbolicLink = 75
		LongNameRegularFile = 76
	#tag EndEnum


	#tag ViewBehavior
		#tag ViewProperty
			Name="ignoreChecksumErrors"
			Group="Behavior"
			InitialValue="0"
			Type="boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="ignoreExistingFiles"
			Group="Behavior"
			InitialValue="0"
			Type="boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="ignoreMacContent"
			Group="Behavior"
			InitialValue="0"
			Type="boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="ignoreSpecialFiles"
			Group="Behavior"
			InitialValue="0"
			Type="boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="overwriteFilesOnExpanding"
			Group="Behavior"
			InitialValue="0"
			Type="boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
