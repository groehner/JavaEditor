{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterSQL.pas, released 2000-04-21.
The Original Code is based on the wmSQLSyn.pas and wmSybaseSyn.pas files from
the mwEdit component suite by Martin Waldenburg and other developers, the
Initial Author of these files is Willo van der Merwe. Initial Author of
SynHighlighterSQL.pas is Michael Hieke.
Portions created by Willo van der Merwe are Copyright 1999 Willo van der Merwe.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterSQL.pas,v 1.39.2.14 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(SQL highlighter for SynEdit with support for different dialects.)
@author(Michael Hieke)
@created(2000-04-21)
@lastmod(2000-11-16)
The SynHighlighterSQL implements a highlighter for SQL for the SynEdit projects.
Different SQL dialects can be selected via the Dialect property.
}

unit SynHighlighterSQL;

{$I SynEdit.inc}

interface

uses
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SynUnicode,
  Generics.Collections,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkDatatype, tkDefaultPackage, tkException,
    tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkPLSQL,
    tkSQLPlus, tkString, tkSymbol, tkTableName, tkUnknown, tkVariable,
    tkConditionalComment, tkDelimitedIdentifier, tkProcName, tkConsoleOutput);

  TRangeState = (rsUnknown, rsComment, rsString, rsConditionalComment, rsConsoleOutput);

  TSQLDialect = (sqlStandard, sqlInterbase6, sqlMSSQL7, sqlMySQL, sqlOracle,
    sqlSybase, sqlIngres, sqlMSSQL2K, sqlPostgres, sqlNexus, sqlInformix);

type
  TSynSQLSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fKeywords: TSynHashEntryList;
    FProcNames: TStrings;
    fTableNames: TStrings;
    FTableDict: TDictionary<string, Boolean>;
    fFunctionNames: TStrings;
    fDialect: TSQLDialect;
    fCommentAttri: TSynHighlighterAttributes;
    fConditionalCommentAttri: TSynHighlighterAttributes;
    fConsoleOutputAttri: TSynHighlighterAttributes;
    fDataTypeAttri: TSynHighlighterAttributes;
    fDefaultPackageAttri: TSynHighlighterAttributes;
    fDelimitedIdentifierAttri: TSynHighlighterAttributes;
    fExceptionAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fPLSQLAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSQLPlusAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTableNameAttri: TSynHighlighterAttributes;
    fProcNameAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure DoAddKeyword(AKeyword: string; AKind: Integer);
    procedure SetDialect(Value: TSQLDialect);
    procedure SetTableNames(const Value: TStrings);
    procedure SetFunctionNames(const Value: TStrings);
    procedure PutFunctionNamesInKeywordList;
    procedure FunctionNamesChanged(Sender: TObject);
    procedure ProcNamesChanged(Sender: TObject);
    procedure TableNamesChanged(Sender: TObject);
    procedure InitializeKeywordLists;
    procedure PutProcNamesInKeywordList;
    procedure PutTableNamesInKeywordList;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure HashProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure QuoteProc;
    procedure BacktickProc;
    procedure BracketProc;
    procedure SymbolProc;
    procedure SymbolAssignProc;
    procedure VariableProc;
    procedure UnknownProc;
    procedure AnsiCProc;
    procedure SetProcNames(const Value: TStrings);
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): string; override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsKeyword(const AKeyword: string): Boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property ConditionalCommentAttri: TSynHighlighterAttributes
      read fConditionalCommentAttri write fConditionalCommentAttri;
    property ConsoleOutputAttri: TSynHighlighterAttributes
      read FConsoleOutputAttri write FConsoleOutputAttri;
    property DataTypeAttri: TSynHighlighterAttributes read fDataTypeAttri
      write fDataTypeAttri;
    property DefaultPackageAttri: TSynHighlighterAttributes
      read fDefaultPackageAttri write fDefaultPackageAttri;
    property DelimitedIdentifierAttri: TSynHighlighterAttributes
      read fDelimitedIdentifierAttri write fDelimitedIdentifierAttri;
    property ExceptionAttri: TSynHighlighterAttributes read fExceptionAttri
      write fExceptionAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property PLSQLAttri: TSynHighlighterAttributes read fPLSQLAttri
      write fPLSQLAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SQLPlusAttri: TSynHighlighterAttributes read fSQLPlusAttri
      write fSQLPlusAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property ProcNameAttri: TSynHighlighterAttributes read FProcNameAttri
      write FProcNameAttri;
    property TableNameAttri: TSynHighlighterAttributes read fTableNameAttri
      write fTableNameAttri;
    property ProcNames: TStrings read FProcNames write SetProcNames;
    property TableNames: TStrings read fTableNames write SetTableNames;
    property FunctionNames: TStrings read fFunctionNames write SetFunctionNames;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
    property SQLDialect: TSQLDialect read fDialect write SetDialect
      default sqlStandard;
  end;

implementation

uses
  SynEditStrConst;

const
//---"Standard" (ANSI SQL keywords (Version 1, 2 and 3) (www.sql.org)-----------
  StandardKW: string =
    'absolute,action,active,actor,add,after,alias,all,allocate,alter,' +
    'and,any,are,as,asc,ascending,assertion,async,at,attributes,auto,' +
    'base_name,before,begin,between,bit,bit_length,boolean,both,breadth,by,' +
    'cache,call,cascade,cascaded,case,cast,catalog,char_length,' +
    'character_length,check,coalesce,collate,collation,column,commit,' +
    'committed,completion,computed,conditional,connect,connection,constraint,' +
    'constraints,containing,convert,corresponding,count,create,cross,current,' +
    'current_date,current_path,current_time,current_timestamp,current_user,' +
    'cursor,cycle,data,database,date,day,deallocate,debug,declare,default,' +
    'deferrable,deferred,delete,depth,desc,descending,describe,descriptor,' +
    'destroy,diagnostics,dictionary,disconnect,distinct,do,domain,' +
    'drop,each,element,else,elseif,end,end-exec,entry_point,equals,escape,' +
    'except,exception,execute,exists,exit,external,extract,factor,false,' +
    'filter,first,for,foreign,from,full,function,general,generator,get,' +
    'global,grant,group,having,hold,hour,identity,if,ignore,immediate,in,' +
    'inactive,index,initially,inner,input,insensitive,insert,instead,' +
    'intersect,interval,into,is,isolation,join,key,last,leading,leave,left,' +
    'less,level,like,limit,list,local,loop,lower,match,merge,minute,modify,' +
    'month,names,national,natural,nchar,new,new_table,next,no,none,not,null,' +
    'nullif,object,octet_length,of,off,old,old_table,on,only,operation,' +
    'operator,operators,or,order,others,outer,output,overlaps,pad,' +
    'parameter,parameters,partial,password,path,pendant,plan,position,' +
    'postfix,prefix,preorder,prepare,preserve,primary,prior,private,' +
    'privileges,procedure,protected,read,recursive,ref,referencing,relative,' +
    'replace,resignal,restrict,retain,return,returns,revoke,right,role,' +
    'rollback,routine,row,rows,savepoint,schema,scroll,search,second,select,' +
    'sensitive,sequence,session,session_user,set,shadow,shared,signal,' +
    'similar,size,snapshot,some,space,sqlexception,sqlstate,sqlwarning,start,' +
    'state,structure,substring,suspend,symbol,system_user,table,temporary,' +
    'term,test,then,there,time,timestamp,timezone_hour,timezone_minute,to,' +
    'trailing,transaction,translate,translation,trigger,trim,true,tuple,type,' +
    'uncommitted,under,union,unique,unknown,update,upper,usage,user,using,' +
    'value,varchar,variable,varying,view,virtual,visible,wait,when,where,' +
    'while,with,without,work,write,year,zone';

//---Sybase keywords------------------------------------------------------------
  SybaseKW: string =
    'absolute,action,add,after,alias,all,allocate,alter,and,any,are,' +
    'arith_overflow,as,asc,assertion,async,at,authorization,avg,before,begin,' +
    'between,bit,bit_length,boolean,both,breadth,break,browse,bulk,by,call,' +
    'cascade,cascaded,case,cast,catalog,char,char_convert,char_length,' +
    'character,character_length,check,checkpoint,close,clustered,coalesce,' +
    'collate,collation,column,commit,completion,compute,confirm,' +
    'connect,connection,constraint,constraints,continue,controlrow,convert,' +
    'corresponding,count,create,cross,current,current_date,current_time,' +
    'current_timestamp,current_user,cursor,cycle,data,database,date,day,dbcc,' +
    'deallocate,dec,decimal,declare,default,deferrable,deferred,delete,depth,' +
    'desc,describe,descriptor,diagnostics,dictionary,dis,disconnect,distinct,' +
    'domain,double,drop,dummy,dump,each,else,elseif,en,end,endtran,equals,' +
    'errlvl,errordata,errorexit,escape,except,exception,exclusive,exec,' +
    'execute,exists,exit,exp_row_size,external,extract,false,fetch,' +
    'fillfactor,first,float,for,foreign,found,from,full,general,get,global,' +
    'go,goto,grant,group,having,holdlock,hour,identity,identity_gap,' +
    'identity_insert,identity_start,if,ignore,immediate,in,index,indicator,' +
    'initially,inner,input,insensitive,insert,install,int,integer,intersect,' +
    'interval,into,is,isolation,jar,join,key,kill,language,last,leading,' +
    'leave,left,less,level,like,limit,lineno,load,local,lock,loop,lower,' +
    'match,max,max_rows_per_page,min,minute,mirror,mirrorexit,modify,module,' +
    'month,names,national,natural,nchar,new,next,no,noholdlock,nonclustered,' +
    'none,not,null,nullif,numeric,numeric_truncation,object,' +
    'octet_length,of,off,offsets,oid,old,on,once,online,only,open,operation,' +
    'operators,option,or,order,others,outer,output,over,overlaps,pad,' +
    'parameters,partial,partition,pendant,perm,permanent,plan,position,' +
    'precision,preorder,prepare,preserve,primary,print,prior,private,' +
    'privileges,proc,procedure,processexit,protected,proxy_table,public,' +
    'quiesce,raiserror,read,readpast,readtext,real,reconfigure,recursive,' +
    'ref,reference,referencing,relative,remove,reorg,replace,replication,' +
    'reservepagegap,resignal,restrict,return,returns,revoke,right,role,' +
    'rollback,routine,row,rowcount,rows,rule,save,savepoint,schema,scroll,' +
    'search,second,section,select,sensitive,sequence,session_user,set,' +
    'setuser,shared,shutdown,signal,similar,size,smallint,some,space,sql,' +
    'sqlcode,sqlerror,sqlexception,sqlstate,statistics,stripe,structure,' +
    'substring,sum,syb_identity,syb_restree,system_user,table,temp,temporary,' +
    'test,textsize,then,there,time,timestamp,timezone_hour,timezone_minute,' +
    'to,trailing,tran,transaction,translate,translation,trigger,trim,true,' +
    'truncate,tsequal,type,under,union,unique,unknown,unpartition,update,' +
    'upper,usage,use,user,user_option,using,value,values,varchar,variable,' +
    'varying,view,virtual,visible,wait,waitfor,when,whenever,where,while,' +
    'with,without,work,write,writetext,year,zone';

//---Oracle---------------------------------------------------------------------
  // Oracle SQL keywords
  OracleKW: string =
    'ACCESS,ACCESSED,ACCOUNT,ACTIVATE,ACTIVE_INSTANCE_COUNT,ADD,ADMIN,ADVISE,' +
    'AGENT,ALL,ALLOCATE,ALTER,ANALYZE,ANCILLARY,AND,ANY,AQ_TM_PROCESSES,' +
    'ARCHIVE_LAG_TARGET,ARCHIVELOG,AS,ASC,ASSOCIATE,ATTRIBUTES,AUDIT,' +
    'AUDIT_FILE_DEST,AUDIT_SYS_OPERATIONS,AUDIT_TRAIL,AUTHENTICATED,AUTHID,' +
    'AUTOALLOCATE,AUTOEXTEND,AUTOMATIC,BACKGROUND_CORE_DUMP,' +
    'BACKGROUND_DUMP_DEST,BACKUP,BACKUP_TAPE_IO_SLAVES,BECOME,BEFORE,' +
    'BEHALF,BETWEEN,BINDING,BITMAP,BITMAP_MERGE_AREA_SIZE,BLANK_TRIMMING,' +
    'BLOCK,BLOCKSIZE,BUFFER_POOL,BUFFER_POOL_KEEP,BUFFER_POOL_RECYCLE,BY,' +
    'CACHE,CANCEL,CASCADE,CAST,CATEGORY,CHAINED,CHANGE,CHARACTER,CHECK,' +
    'CHECKPOINT,CHILD,CHUNK,CIRCUITS,CLASS,CLONE,CLUSTER,CLUSTER_DATABASE,' +
    'CLUSTER_DATABASE_INSTANCES,CLUSTER_INTERCONNECTS,COALESCE,COBOL,' +
    'COLUMN,COLUMNS,COMMENT,COMMIT_POINT_STRENGTH,COMPATIBLE,COMPILE,' +
    'COMPLETE,COMPOSITE_LIMIT,COMPRESS,COMPUTE,CONNECT,' +
    'CONNECT_TIME,CONSIDER,CONSTRAINT,CONSTRAINTS,CONTENTS,CONTEXT,CONTINUE,' +
    'CONTROL,CONTROL_FILE_RECORD_KEEP_TIME,CONTROL_FILES,CONTROLFILE,' +
    'CORE_DUMP_DEST,COST,CPU_COUNT,CPU_PER_CALL,CPU_PER_SESSION,CREATE,' +
    'CREATE_BITMAP_AREA_SIZE,CREATE_STORED_OUTLINES,CURRENT,CURRENT_USER,' +
    'CURSOR_SHARING,CURSOR_SPACE_FOR_TIME,CYCLE,DANGLING,DATAFILE,' +
    'DB_BLOCK_BUFFERS,DB_BLOCK_CHECKING,DB_BLOCK_CHECKSUM,DB_BLOCK_SIZE,' +
    'DB_CACHE_ADVICE,DB_CACHE_SIZE,DB_CREATE_FILE_DEST,DB_DOMAIN,' +
    'DB_FILE_MULTIBLOCK_READ_COUNT,DB_FILE_NAME_CONVERT,DB_FILES,' +
    'DB_KEEP_CACHE_SIZE,DB_NAME,DB_RECYCLE_CACHE_SIZE,DB_WRITER_PROCESSES,' +
    'DBLINK_ENCRYPT_LOGIN,DBWR_IO_SLAVES,DEALLOCATE,DEBUG,DEFAULT,DEFERRED,' +
    'DEFINER,DELETE,DEMAND,DETERMINES,DG_BROKER_START,DICTIONARY,DIMENSION,' +
    'DIRECTORY,DISABLE,DISASSOCIATE,DISK_ASYNCH_IO,DISMOUNT,DISPATCHERS,' +
    'DISTINCT,DISTRIBUTED,DISTRIBUTED_LOCK_TIMEOUT,DML,DML_LOCKS,DOCUMENT,' +
    'DROP,DRS_START,ELSE,ENABLE,ENQUEUE_RESOURCES,ESCAPE,ESTIMATE,EVENT,' +
    'EVENTS,EXCEPT,EXCEPTIONS,EXCHANGE,EXCLUDING,EXCLUSIVE,EXISTS,EXPIRE,' +
    'EXPLAIN,EXTENT,EXTERNALLY,FAILED_LOGIN_ATTEMPTS,FAL_CLIENT,FAL_SERVER,' +
    'FAST,FAST_START_IO_TARGET,FAST_START_MTTR_TARGET,' +
    'FAST_START_PARALLEL_ROLLBACK,FILE,FILE_MAPPING,FILESYSTEMIO_OPTIONS,' +
    'FIXED_DATE,FLUSH,FOR,FORCE,FOREIGN,FORTRAN,FREELIST,FREELISTS,FRESH,' +
    'FROM,FROM_TZ,FUNCTIONS,FULL,GC_FILES_TO_LOCKS,GENERATED,GLOBAL,' +
    'GLOBAL_CONTEXT_POOL_SIZE,GLOBAL_NAME,GLOBAL_NAMES,GLOBALLY,GO,GRANT,' +
    'GROUP,GROUPS,HASH,HASH_AREA_SIZE,HASH_JOIN_ENABLED,HASHKEYS,HAVING,HEAP,' +
    'HI_SHARED_MEMORY_ADDRESS,HIERARCHY,HS_AUTOREGISTER,IDENTIFIED,IDLE_TIME,' +
    'IFILE,IMMEDIATE,IN,INCLUDING,INCREMENT,INDEX,INDEXTYPE,INDEXTYPES,' +
    'INFILE,INITIAL,INITIALIZED,INITIALLY,INITRANS,INSERT,INSTANCE,' +
    'INSTANCE_GROUPS,INSTANCE_NAME,INSTANCE_NUMBER,INT,INTERSECT,INTO,' +
    'INVALIDATE,IS,ISOLATION,JAVA,JAVA_MAX_SESSIONSPACE_SIZE,JAVA_POOL_SIZE,' +
    'JAVA_SOFT_SESSIONSPACE_LIMIT,JOB_QUEUE_PROCESSES,JOIN,KEEP,KEY,KILL,' +
    'LARGE_POOL_SIZE,LAYERLISTS,LEFT,LEVEL,LIBRARY,LICENSE_MAX_SESSIONS,' +
    'LICENSE_MAX_USERS,LICENSE_SESSIONS_WARNING,LIKE,LIMIT,LINK,LIST,LOB,' +
    'LOCAL,LOCAL_LISTENER,LOCATOR,LOCK,LOCK_NAME_SPACE,LOCK_SGA,' +
    'LOG_ARCHIVE_DEST,LOG_ARCHIVE_DUPLEX_DEST,LOG_ARCHIVE_FORMAT,' +
    'LOG_ARCHIVE_MAX_PROCESSES,LOG_ARCHIVE_MIN_SUCCEED_DEST,' +
    'LOG_ARCHIVE_START,LOG_ARCHIVE_TRACE,LOG_BUFFER,LOG_CHECKPOINT_INTERVAL,' +
    'LOG_CHECKPOINT_TIMEOUT,LOG_CHECKPOINTS_TO_ALERT,LOG_FILE_NAME_CONVERT,' +
    'LOG_PARALLELISM,LOGFILE,LOGGING,LOGICAL_READS_PER_CALL,' +
    'LOGICAL_READS_PER_SESSION,LOGMNR_MAX_PERSISTENT_SESSIONS,MANAGE,MANAGED,' +
    'MANAGEMENT,MANUAL,MAP,MASTER,MATCHED,MATERIALIZED,MAX_COMMIT_PROPAGATION_DELAY,' +
    'MAX_DISPATCHERS,MAX_DUMP_FILE_SIZE,MAX_ENABLED_ROLES,' +
    'MAX_ROLLBACK_SEGMENTS,MAX_SHARED_SERVERS,MAXDATAFILES,MAXEXTENTS,' +
    'MAXINSTANCES,MAXLOGFILES,MAXLOGHISTORY,MAXLOGMEMBERS,MAXSIZE,MAXTRANS,' +
    'MAXVALUE,MEMBER,MERGE,MINEXTENTS,MINIMIZE,MINIMUM,MINUS,MINVALUE,MODE,' +
    'MODIFY,MODULE,MONITORING,MOUNT,MOVE,MOVEMENT,MULTISET,NAMED,NATIONAL,' +
    'NESTED,NEVER,NEXT,NLS_CALENDAR,NLS_COMP,NLS_CURRENCY,NLS_DATE_FORMAT,' +
    'NLS_DATE_LANGUAGE,NLS_DUAL_CURRENCY,NLS_ISO_CURRENCY,NLS_LANGUAGE,' +
    'NLS_LENGTH_SEMANTICS,NLS_NCHAR_CONV_EXCP,NLS_NUMERIC_CHARACTERS,' +
    'NLS_TERRITORY,NLS_TIMESTAMP_FORMAT,NLS_TIMESTAMP_TZ_FORMAT,NO,' +
    'NOARCHIVELOG,NOAUDIT,NOCACHE,NOCOMPRESS,NOCOPY,NOCYCLE,NOFORCE,' +
    'NOLOGGING,NOMAXVALUE,NOMINIMIZE,NOMINVALUE,NOMONITORING,NOMOUNT,NONE,' +
    'NOORDER,NORELY,NORESETLOGS,NOREVERSE,NORMAL,NOROWDEPENDENCIES,NOSORT,' +
    'NOT,NOTHING,NOVALIDATE,NOWAIT,NULL,O7_DICTIONARY_ACCESSIBILITY,' +
    'OBJECT_CACHE_MAX_SIZE_PERCENT,OBJECT_CACHE_OPTIMAL_SIZE,OF,OFFLINE,OID,' +
    'OLAP_PAGE_POOL_SIZE,ON,ONLINE,ONLY,OPEN_CURSORS,OPEN_LINKS,' +
    'OPEN_LINKS_PER_INSTANCE,OPERATOR,OPTIMAL,OPTIMIZER_DYNAMIC_SAMPLING,' +
    'OPTIMIZER_FEATURES_ENABLE,OPTIMIZER_INDEX_CACHING,' +
    'OPTIMIZER_INDEX_COST_ADJ,OPTIMIZER_MAX_PERMUTATIONS,OPTIMIZER_MODE,' +
    'OPTION,OR,ORACLE_TRACE_COLLECTION_NAME,ORACLE_TRACE_COLLECTION_PATH,' +
    'ORACLE_TRACE_COLLECTION_SIZE,ORACLE_TRACE_ENABLE,' +
    'ORACLE_TRACE_FACILITY_NAME,ORACLE_TRACE_FACILITY_PATH,ORDER,' +
    'OS_AUTHENT_PREFIX,OS_ROLES,OUTER,OUTLINE,OVERFLOW,OWN,PACKAGES,PARALLEL,' +
    'PARALLEL_ADAPTIVE_MULTI_USER,PARALLEL_AUTOMATIC_TUNING,' +
    'PARALLEL_EXECUTION_MESSAGE_SIZE,PARALLEL_INSTANCE_GROUP,' +
    'PARALLEL_MAX_SERVERS,PARALLEL_MIN_PERCENT,PARALLEL_MIN_SERVERS,' +
    'PARALLEL_THREADS_PER_CPU,PARAMETERS,PARTITION_VIEW_ENABLED,PARTITIONS,' +
    'PASSWORD,PASSWORD_GRACE_TIME,PASSWORD_LIFE_TIME,PASSWORD_LOCK_TIME,' +
    'PASSWORD_REUSE_MAX,PASSWORD_REUSE_TIME,PASSWORD_VERIFY_FUNCTION,' +
    'PCTFREE,PCTINCREASE,PCTTHRESHOLD,PCTUSED,PCTVERSION,PERCENT,PERMANENT,' +
    'PGA_AGGREGATE_TARGET,PIPELINED,PLAN,PLI,PLSQL_COMPILER_FLAGS,' +
    'PLSQL_NATIVE_C_COMPILER,PLSQL_NATIVE_LIBRARY_DIR,' +
    'PLSQL_NATIVE_LIBRARY_SUBDIR_COUNT,PLSQL_NATIVE_LINKER,' +
    'PLSQL_NATIVE_MAKE_FILE_NAME,PLSQL_NATIVE_MAKE_UTILITY,' +
    'PLSQL_V2_COMPATIBILITY,POST_TRANSACTION,PRE_PAGE_SGA,PREBUILD,PRECISION,' +
    'PRIMARY,PRIOR,PRIVATE_SGA,PRIVILEGES,PROCESSES,PROFILE,PUBLIC,QUERY,' +
    'QUERY_REWRITE_ENABLED,QUERY_REWRITE_INTEGRITY,QUIESCE,QUOTA,' +
    'RDBMS_SERVER_DN,READ,READ_ONLY_OPEN_DELAYED,REBUILD,RECORDS_PER_BLOCK,' +
    'RECOVER,RECOVERABLE,RECOVERY,RECOVERY_PARALLELISM,RECYCLE,RECYCLEBIN,REDUCED,' +
    'REFERENCES,REFRESH,REGISTER,RELY,REMOTE_ARCHIVE_ENABLE,' +
    'REMOTE_DEPENDENCIES_MODE,REMOTE_LISTENER,REMOTE_LOGIN_PASSWORDFILE,' +
    'REMOTE_OS_AUTHENT,REMOTE_OS_ROLES,RENAME,' +
    'REPLICATION_DEPENDENCY_TRACKING,RESET,RESETLOGS,RESIZE,RESOLVE,RESOLVER,' +
    'RESOURCE,RESOURCE_LIMIT,RESOURCE_MANAGER_PLAN,RESTRICT,RESTRICTED,' +
    'RESUMABLE,RESUME,REUSE,REVOKE,REWRITE,RIGHT,RNDS,RNPS,ROLE,ROLES,' +
    'ROLLBACK_SEGMENTS,ROW,ROW_LOCKING,ROWDEPENDENCIES,ROWLABEL,ROWNUM,' +
    'ROWS,SAMPLE,SCN,SCOPE,SECTION,SEGMENT,SELECT,SELECTIVITY,SEQUENCE,' +
    'SERIAL_REUSE,SERVICE_NAMES,SESSION,SESSION_CACHED_CURSORS,' +
    'SESSION_MAX_OPEN_FILES,SESSIONS,SESSIONS_PER_USER,SGA_MAX_SIZE,' +
    'SHADOW_CORE_DUMP,SHARE,SHARED,SHARED_MEMORY_ADDRESS,SHARED_POOL,' +
    'SHARED_POOL_RESERVED_SIZE,SHARED_POOL_SIZE,SHARED_SERVER_SESSIONS,' +
    'SHARED_SERVERS,SHRINK,SIZE,SNAPSHOT,SOME,SORT,SORT_AREA_RETAINED_SIZE,' +
    'SORT_AREA_SIZE,SOURCE,SPECIFICATION,SPECIFIED,SPFILE,SPLIT,SQL_TRACE,' +
    'SQL92_SECURITY,STANDBY,STANDBY_ARCHIVE_DEST,STANDBY_FILE_MANAGEMENT,' +
    'STAR_TRANSFORMATION_ENABLED,START,START_DATE,STATIC,STATISTICS,' +
    'STATISTICS_LEVEL,STOP,STORAGE,STRUCTURE,SUBPARTITION,SUBPARTITIONS,' +
    'SUCCESSFUL,SUSPEND,SWITCH,SYNONYM,SYSTEM,TABLE,TABLESPACE,' +
    'TAPE_ASYNCH_IO,TEMPFILE,TEMPORARY,THE,THEN,THREAD,THROUGH,TIME,' +
    'TIMED_OS_STATISTICS,TIMED_STATISTICS,TIMEOUT,TO,TRACE_ENABLED,' +
    'TRACEFILE_IDENTIFIER,TRACING,TRANSACTION,TRANSACTION_AUDITING,' +
    'TRANSACTIONS,TRANSACTIONS_PER_ROLLBACK_SEGMENT,TRIGGER,TRUNCATE,TRUST,' +
    'TYPES,UNARCHIVED,UNDER,UNDO,UNDO_MANAGEMENT,UNDO_RETENTION,' +
    'UNDO_SUPPRESS_ERRORS,UNDO_TABLESPACE,UNIFORM,UNION,UNIQUE,UNLIMITED,' +
    'UNLOCK,UNQUIESCE,UNRECOVERABLE,UNTIL,UNUSABLE,UNUSED,UPDATE,USAGE,' +
    'USE_INDIRECT_DATA_BUFFERS,USER_DUMP_DEST,VALIDATE,VALIDATION,VALUES,' +
    'VARGRAPHIC,VARRAY,VIEW,WHERE,WITH,WITHOUT,WNDS,WNPS,' +
    'WORKAREA_SIZE_POLICY';

//---Postgresql-----------------------------------------------------------------
  //Postgresql Keywords
  PostgresKW: string =
    'IF,LOOP,ABORT,ABSOLUTE,ACCESS,ACTION,ADA,ADD,ADMIN,AFTER,AGGREGATE,ALIAS' +
    ',ALLOCATE,ALTER,ANALYSE,ANALYZE,AND,ARE,AS,ASC,ASENSITIVE' +
    ',ASSERTION,ASSIGNMENT,ASYMMETRIC,AT,ATOMIC,AUTHORIZATION,BACKWARD' +
    ',BEFORE,BEGIN,BETWEEN' +
    ',BOTH,BREADTH,BY,C,CACHE,CALL,CALLED,CARDINALITY,CASCADE,CASCADED,CASE' +
    ',CAST,CATALOG,CATALOG_NAME,CHAIN,CHARACTERISTICS' +
    ',CHARACTER_SET_CATALOG,CHARACTER_SET_NAME,CHARACTER_SET_SCHEMA' +
    ',CHECK,CHECKED,CHECKPOINT,CLASS,CLASS_ORIGIN,CLOB,CLOSE,CLUSTER,COBOL,COLLATE' +
    ',COLLATION,COLLATION_CATALOG,COLLATION_NAME,COLLATION_SCHEMA,COLUMN,COLUMN_NAME' +
    ',COMMAND_Function,COMMAND_Function_CODE,COMMENT,COMMIT,COMMITTED,COMPLETION' +
    ',CONDITION_NUMBER,CONNECT,CONNECTION,CONNECTION_NAME,CONSTRAINT,CONSTRAINTS' +
    ',CONSTRAINT_CATALOG,CONSTRAINT_NAME,CONSTRAINT_SCHEMA,CONSTRUCTOR,CONTAINS' +
    ',CONTINUE,CONVERSION,COPY,CORRESPONDING,CREATE,CREATEDB,CREATEUSER' +
    ',CROSS,CUBE,CURRENT,CURRENT_PATH,CURRENT_ROLE' +
    ',CURSOR,CURSOR_NAME,CYCLE,DATA,DATABASE,DATETIME_INTERVAL_CODE' +
    ',DATETIME_INTERVAL_PRECISION,DAY,DEALLOCATE,DEC,DECLARE,DEFAULT,DEFERRABLE' +
    ',DEFERRED,DEFINED,DEFINER,DELETE,DELIMITER,DELIMITERS,DEPTH,DEREF,DESC,DESCRIBE' +
    ',DESCRIPTOR,DESTROY,DESTRUCTOR,DETERMINISTIC,DIAGNOSTICS,DICTIONARY,DISCONNECT' +
    ',DISPATCH,DISTINCT,DO,DOMAIN,DROP,DYNAMIC,DYNAMIC_Function,DYNAMIC_Function_CODE' +
    ',EACH,ELSE,ELSIF,ELSEIF,ENCODING,ENCRYPTED,END,EQUALS,ESCAPE,EXCEPT,EXCEPTION' +
    ',EXCLUSIVE,EXEC,EXECUTE,EXISTING,EXPLAIN,EXTERNAL,FALSE,FETCH' +
    ',FINAL,FIRST,FOR,FORCE,FOREIGN,FORTRAN,FORWARD,FOUND,FREE,FREEZE,FROM' +
    ',FULL,Function,G,GENERAL,GENERATED,GET,GLOBAL,GO,GOTO,GRANT,GRANTED,GROUP' +
    ',GROUPING,HANDLER,HAVING,HIERARCHY,HOLD,HOUR,IDENTITY,IGNORE,ILIKE' +
    ',IMMEDIATE,IMMUTABLE,IMPLEMENTATION,IMPLICIT,INCREMENT,INDEX,INDICATOR' +
    ',INFIX,INHERITS,INITIALIZE,INITIALLY,INNER,INOUT,INPUT,INSENSITIVE,INSERT' +
    ',INSTANCE,INSTANTIABLE,INSTEAD,INT,INTERSECT,INTO,INVOKER' +
    ',IS,ISNULL,ISOLATION,ITERATE,JOIN,K,KEY,KEY_MEMBER,KEY_TYPE,LANCOMPILER,LANGUAGE' +
    ',LARGE,LAST,LATERAL,LEADING,LEFT,LESS,LEVEL,LIKE,LIMIT,LISTEN,LOAD,LOCAL' +
    ' LOCATION,LOCATOR,LOCK,M,MAP,MATCH,MAXVALUE,MESSAGE_LENGTH' +
    ',MESSAGE_OCTET_LENGTH,MESSAGE_TEXT,METHOD,MINUTE,MINVALUE,MODE,MODIFIES' +
    ',MODIFY,MODULE,MONTH,MORE,MOVE,MUMPS,NAME,NAMES,NATIONAL,NATURAL,NCHAR,NCLOB' +
    ',NEW,NEXT,NO,NOCREATEDB,NOCREATEUSER,NONE,NOT,NOTHING,NOTIFY,NOTNULL,NULL,NULLABLE' +
    ',NUMBER,OBJECT,OF,OFF,OFFSET,OIDS,OLD,ON,ONLY,OPEN' +
    ',OPERATION,Operator,OPTION,OPTIONS,OR,ORDER,ORDINALITY,OUT,OUTER,OUTPUT,OVERLAPS' +
    ',OVERRIDING,OWNER,PAD,PARAMETER,PARAMETERS,PARAMETER_MODE,PARAMETER_NAME,PARAMETER_ORDINAL_POSITION' +
    ',PARAMETER_SPECIFIC_CATALOG,PARAMETER_SPECIFIC_NAME,PARAMETER_SPECIFIC_SCHEMA,PARTIAL,PASCAL,PASSWORD' +
    ',PENDANT,PLACING,PLI,POSTFIX,PRECISION,PREFIX,PREORDER,PREPARE,PRESERVE,PRIMARY' +
    ',PRIOR,PRIVILEGES,PROCEDURAL,PROCEDURE,PUBLIC,READ,READS,RECHECK,RECURSIVE,REF,REFERENCES' +
    ',REFERENCING,REINDEX,RELATIVE,RENAME ,REPEATABLE,RESET,RESTRICT,RESULT,RETURN,RETURNED_LENGTH' +
    ',RETURNED_OCTET_LENGTH,RETURNED_SQLSTATE,RETURNS,REVOKE,RIGHT,ROLE,ROLLBACK,ROLLUP,ROUTINE,ROUTINE_CATALOG' +
    ',ROUTINE_NAME,ROUTINE_SCHEMA,ROW,ROWS,ROW_COUNT,RULE,SAVEPOINT,SCALE,SCHEMA,SCHEMA_NAME' +
    ',SCOPE,SCROLL,SEARCH,SECOND,SECTION,SECURITY,SELECT,SELF,SENSITIVE,SEQUENCE,SERIALIZABLE,SERVER_NAME' +
    ',SESSION,SET,SETOF,SETS,SHARE,SHOW,SIMILAR,SIMPLE,SIZE,SOURCE,SPACE' +
    ',SPECIFIC,SPECIFICTYPE,SPECIFIC_NAME,SQLCODE,SQLERROR,SQLEXCEPTION,SQLSTATE,SQLWARNING' +
    ',STABLE,START,STATE,STATEMENT,STATIC,STATISTICS,STDIN,STDOUT,STORAGE,STRICT,STRUCTURE' +
    ',STYLE,SUBCLASS_ORIGIN,SUBLIST,SYMMETRIC,SYSID,SYSTEM,SYSTEM_USER,TABLE' +
    ',TABLE_NAME,TEMP,TEMPLATE,TEMPORARY,TERMINATE,THAN,THEN,TIMEZONE_HOUR' +
    ',TIMEZONE_MINUTE,TO,TOAST,TRAILING,TRANSACTION,TRANSACTIONS_COMMITTED,TRANSACTIONS_ROLLED_BACK' +
    ',TRANSACTION_ACTIVE,TRANSFORM,TRANSFORMS,TRANSLATION,TREAT,TRIGGER_CATALOG' +
    ',TRIGGER_NAME,TRIGGER_SCHEMA,TRUE,TRUNCATE,TRUSTED,TYPE,UNCOMMITTED,UNDER,UNENCRYPTED,UNION'+
    ',UNIQUE,UNKNOWN,UNLISTEN,UNNAMED,UNNEST,UNTIL,UPDATE,USAGE,USER_DEFINED_TYPE_CATALOG' +
    ',USER_DEFINED_TYPE_NAME,USER_DEFINED_TYPE_SCHEMA,USING,VACUUM,VALID,VALIDATOR,VALUE,VALUES' +
    ',VARIABLE,VARYING,VERBOSE,VIEW,VOLATILE,WHEN,WHENEVER,WHERE,WITH,WITHOUT,WORK,WRITE,YEAR,ZONE';

  //Postgresql Functions
  PostgresFunctions: string =
    'abs,cbrt,ceil,ceiling,degrees,exp,floor,ln,log,mod,pi,power,radians,random,'+
    'round,setseed,sign,sqrt,trunc,width_bucket,acos,asin,atan,atan2,cos,cot,'+
    'sin,tan,bit_length,char_length,character_length,convert,lower,octet_length,'+
    'overlay,position,substring,trim,upper,ascii,btrim,chr,decode,'+
    'encode,initcap,length,lpad,ltrim,md5,pg_client_encoding,quote_ident,quote_literal,'+
    'replace,rpad,rtrim,split_part,strpos,substr,to_ascii,to_hex,translate,get_byte,'+
    'set_byte,get_bit,set_bit,to_char,to_date,'+
    'to_timestamp,to_number,age,date_part,date_trunc,extract,now,'+
    'timeofday,isfinite,area,box_intersect,center,diameter,height,isclosed,isopen,'+
    'npoints,pclose,popen,radius,width,'+
    'broadcast,'+
    'host,masklen,set_masklen,netmask,hostmask,network,abbrev,family,nextval,'+
    'currval,setval,coalesce,nullif,array_cat ,array_append ,array_prepend ,array_dims,'+
    'array_lower ,array_upper ,array_to_string ,string_to_array ,avg,bit_and,bit_or,bool_and,'+
    'bool_or,count,every,max,min,stddev,sum,variance,exists ,in ,some,'+
    'all ,generate_series,current_database,current_schema,'+
    'current_schemas,,inet_client_addr,inet_client_port,inet_server_addr,inet_server_port,'+
    'version,has_table_privilege,has_database_privilege,'+
    'has_function_privilege,has_language_privilege,'+
    'has_schema_privilege,has_tablespace_privilege,'+
    'pg_table_is_visible,pg_type_is_visible,pg_function_is_visible,pg_operator_is_visible,'+
    'pg_opclass_is_visible,pg_conversion_is_visible,format_type,pg_get_viewdef,'+
    'pg_get_ruledef,pg_get_indexdef,'+
    'pg_get_triggerdef,pg_get_constraintdef,pg_get_expr,'+
    'pg_get_userbyid,pg_get_serial_sequence,pg_tablespace_databases,obj_description,'+
    'col_description,current_setting,set_config,pg_cancel_backend,pg_start_backup,pg_stop_backup,'+
    'current_user,current_date,current_time,current_timestamp,localtime,localtimestamp,session_user,user';

  //Postgresql Types
  PostgresTypes: string =
    'smallint,integer,bigint,decimal,numeric,real,double,serial,bigserial,'+
    'character,varchar,char,text,bytea,timestamp, interval,date,'+
    'time,boolean,point,line,lseg,box,path,polygon,circle,cidr,inet,'+
    'macaddr,BIT,bitvar,ARRAY,oid,regproc,regprocedure,regoper,regoperator,regclass,'+
    'regtype,any,anyarray,anyelement,cstring,internal,language_handler,record,'+
    'trigger,void,opaque,refcursor,binary,blob,int4,int2,int8,float,float4,float8';

  //Postgresql Exceptions
  PostgresExceptions: string =
    '$BODY$,SUCCESSFUL_COMPLETION,WARNING,DYNAMIC_RESULT_SETS_RETURNED,IMPLICIT_ZERO_BIT_PADDING,NULL_VALUE_ELIMINATED_IN_SET_FUNCTION,'+
    'PRIVILEGE_NOT_GRANTED,PRIVILEGE_NOT_REVOKED,STRING_DATA_RIGHT_TRUNCATION,DEPRECATED_FEATURE,NO_DATA,NO_ADDITIONAL_DYNAMIC_RESULT_SETS_RETURNED,'+
    'SQL_STATEMENT_NOT_YET_COMPLETE,CONNECTION_EXCEPTION,CONNECTION_DOES_NOT_EXIST,CONNECTION_FAILURE,SQLCLIENT_UNABLE_TO_ESTABLISH_SQLCONNECTION,'+
    'SQLSERVER_REJECTED_ESTABLISHMENT_OF_SQLCONNECTION,TRANSACTION_RESOLUTION_UNKNOWN,PROTOCOL_VIOLATION,TRIGGERED_ACTION_EXCEPTION,'+
    'FEATURE_NOT_SUPPORTED,INVALID_TRANSACTION_INITIATION,LOCATOR_EXCEPTION,INVALID_LOCATOR_SPECIFICATION,INVALID_GRANTOR,INVALID_GRANT_OPERATION,'+
    'INVALID_ROLE_SPECIFICATION,CARDINALITY_VIOLATION,DATA_EXCEPTION,ARRAY_SUBSCRIPT_ERROR,CHARACTER_NOT_IN_REPERTOIRE,DATETIME_FIELD_OVERFLOW,'+
    'DIVISION_BY_ZERO,ERROR_IN_ASSIGNMENT,ESCAPE_CHARACTER_CONFLICT,INDICATOR_OVERFLOW,INTERVAL_FIELD_OVERFLOW,INVALID_ARGUMENT_FOR_LOGARITHM,'+
    'INVALID_ARGUMENT_FOR_POWER_FUNCTION,INVALID_ARGUMENT_FOR_WIDTH_BUCKET_FUNCTION,INVALID_CHARACTER_VALUE_FOR_CAST,INVALID_DATETIME_FORMAT,'+
    'INVALID_ESCAPE_CHARACTER,INVALID_ESCAPE_OCTET,INVALID_ESCAPE_SEQUENCE,INVALID_INDICATOR_PARAMETER_VALUE,INVALID_LIMIT_VALUE,'+
    'INVALID_PARAMETER_VALUE,INVALID_REGULAR_EXPRESSION,INVALID_TIME_ZONE_DISPLACEMENT_VALUE,INVALID_USE_OF_ESCAPE_CHARACTER,'+
    'MOST_SPECIFIC_TYPE_MISMATCH,NULL_VALUE_NOT_ALLOWED,NULL_VALUE_NO_INDICATOR_PARAMETER,NUMERIC_VALUE_OUT_OF_RANGE,STRING_DATA_LENGTH_MISMATCH,'+
    'SUBSTRING_ERROR,TRIM_ERROR,UNTERMINATED_C_STRING,ZERO_LENGTH_CHARACTER_STRING,FLOATING_POINT_EXCEPTION,'+
    'INVALID_TEXT_REPRESENTATION,INVALID_BINARY_REPRESENTATION,BAD_COPY_FILE_FORMAT,UNTRANSLATABLE_CHARACTER,INTEGRITY_CONSTRAINT_VIOLATION,'+
    'RESTRICT_VIOLATION,NOT_NULL_VIOLATION,FOREIGN_KEY_VIOLATION,UNIQUE_VIOLATION,CHECK_VIOLATION,INVALID_CURSOR_STATE,INVALID_TRANSACTION_STATE,'+
    'ACTIVE_SQL_TRANSACTION,BRANCH_TRANSACTION_ALREADY_ACTIVE,HELD_CURSOR_REQUIRES_SAME_ISOLATION_LEVEL,INAPPROPRIATE_ACCESS_MODE_FOR_BRANCH_TRANSACTION,'+
    'INAPPROPRIATE_ISOLATION_LEVEL_FOR_BRANCH_TRANSACTION,NO_ACTIVE_SQL_TRANSACTION_FOR_BRANCH_TRANSACTION,READ_ONLY_SQL_TRANSACTION,'+
    'SCHEMA_AND_DATA_STATEMENT_MIXING_NOT_SUPPORTED,NO_ACTIVE_SQL_TRANSACTION,IN_FAILED_SQL_TRANSACTION,INVALID_SQL_STATEMENT_NAME,TRIGGERED_DATA_CHANGE_VIOLATION,'+
    'INVALID_AUTHORIZATION_SPECIFICATION,DEPENDENT_PRIVILEGE_DESCRIPTORS_STILL_EXIST,DEPENDENT_OBJECTS_STILL_EXIST,INVALID_TRANSACTION_TERMINATION,'+
    'SQL_ROUTINE_EXCEPTION,FUNCTION_EXECUTED_NO_RETURN_STATEMENT,MODIFYING_SQL_DATA_NOT_PERMITTED,PROHIBITED_SQL_STATEMENT_ATTEMPTED,READING_SQL_DATA_NOT_PERMITTED,'+
    'INVALID_CURSOR_NAME,EXTERNAL_ROUTINE_EXCEPTION,CONTAINING_SQL_NOT_PERMITTED,'+
    'EXTERNAL_ROUTINE_INVOCATION_EXCEPTION,INVALID_SQLSTATE_RETURNED,TRIGGER_PROTOCOL_VIOLATED,'+
    'SRF_PROTOCOL_VIOLATED,SAVEPOINT_EXCEPTION,INVALID_SAVEPOINT_SPECIFICATION,INVALID_CATALOG_NAME,INVALID_SCHEMA_NAME,TRANSACTION_ROLLBACK,'+
    'TRANSACTION_INTEGRITY_CONSTRAINT_VIOLATION,SERIALIZATION_FAILURE,STATEMENT_COMPLETION_UNKNOWN,DEADLOCK_DETECTED,SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION,'+
    'SYNTAX_ERROR,INSUFFICIENT_PRIVILEGE,CANNOT_COERCE,GROUPING_ERROR,INVALID_FOREIGN_KEY,INVALID_NAME,NAME_TOO_LONG,RESERVED_NAME,DATATYPE_MISMATCH,'+
    'INDETERMINATE_DATATYPE,WRONG_OBJECT_TYPE,UNDEFINED_COLUMN,UNDEFINED_FUNCTION,UNDEFINED_TABLE,UNDEFINED_PARAMETER,UNDEFINED_OBJECT,'+
    'DUPLICATE_COLUMN,DUPLICATE_CURSOR,DUPLICATE_DATABASE,DUPLICATE_FUNCTION,DUPLICATE_PREPARED_STATEMENT,DUPLICATE_SCHEMA,DUPLICATE_TABLE,'+
    'DUPLICATE_ALIAS,DUPLICATE_OBJECT,AMBIGUOUS_COLUMN,AMBIGUOUS_FUNCTION,AMBIGUOUS_PARAMETER,AMBIGUOUS_ALIAS,INVALID_COLUMN_REFERENCE,'+
    'INVALID_COLUMN_DEFINITION,INVALID_CURSOR_DEFINITION,INVALID_DATABASE_DEFINITION,INVALID_FUNCTION_DEFINITION,INVALID_PREPARED_STATEMENT_DEFINITION,'+
    'INVALID_SCHEMA_DEFINITION,INVALID_TABLE_DEFINITION,INVALID_OBJECT_DEFINITION,WITH_CHECK_OPTION_VIOLATION,INSUFFICIENT_RESOURCES,'+
    'DISK_FULL,OUT_OF_MEMORY,TOO_MANY_CONNECTIONS,PROGRAM_LIMIT_EXCEEDED,STATEMENT_TOO_COMPLEX,TOO_MANY_COLUMNS,TOO_MANY_ARGUMENTS,'+
    'OBJECT_NOT_IN_PREREQUISITE_STATE,OBJECT_IN_USE,CANT_CHANGE_RUNTIME_PARAM,LOCK_NOT_AVAILABLE,OPERATOR_INTERVENTION,QUERY_CANCELED,'+
    'ADMIN_SHUTDOWN,CRASH_SHUTDOWN,CANNOT_CONNECT_NOW,IO_ERROR,UNDEFINED_FILE,DUPLICATE_FILE,CONFIG_FILE_ERROR,LOCK_FILE_EXISTS,'+
    'PLPGSQL_ERROR,RAISE_EXCEPTION,INTERNAL_ERROR,DATA_CORRUPTED,INDEX_CORRUPTED';

  // PLSQL keywords
  OraclePLSQLKW: string =
    'ABORT,ACCEPT,AFTER,ARRAY,ARRAYLEN,ASSERT,ASSIGN,AT,AUTHORIZATION,' +
    'AUTONOMOUS_TRANSACTION,BASE_TABLE,BEGIN,BODY,BULK,BULK_ROWCOUNT,CALL,' +
    'CALLING,CASE,CHAR_BASE,CHARSETFORM,CHARSETID,CLOSE,CLUSTERS,COLAUTH,' +
    'COLLECT,COMMIT,CONNECTION,CONSTANT,COOKIE,COOKIE_TABLE,CRASH,CURRVAL,' +
    'CURSOR,DATA_BASE,DATABASE,DBA,DEBUGOFF,DEBUGON,DECLARE,DEFINITION,' +
    'DELAY,DELTA,DEQUEUE_OPTIONS_T,DETERMINISTIC,DIGITS,DISPOSE,DO,EACH,' +
    'ELSIF,END,ENQUEUE_OPTIONS_T,ENTRY,EXCEPTION,EXCEPTION_INIT,EXIT,' +
    'EXTERNAL,FALSE,FETCH,FIXED,FORALL,FORM,FOUND,FUNCTION,GENERIC,GOTO,IF,' +
    'INDEXES,INDICATOR,INSTEAD,INTERFACE,ISOPEN,LANGUAGE,LCR$_DDL_RECORD,' +
    'LCR$_ROW_LIST,LCR$_ROW_RECORD,LCR$_ROW_UNIT,LIMITED,LOOP,MAXLEN,' +
    'MESSAGE_PROPERTIES_T,MGW_BASIC_MSG_T,MGW_MQSERIES_PROPERTIES,' +
    'MGW_NAME_TYPE_ARRAY_T,MGW_NAME_VALUE_T,MGW_PROPERTIES,MGW_PROPERTY,' +
    'MGW_RAW_VALUE_T,MGW_TEXT_VALUE_T,NAME,NEW,NEXTVAL,NOTFOUND,' +
    'NUMBER_BASE,OLD,OPEN,OUT,PACKAGE,PARALLEL_ENABLE,PARTITION,PASCAL,' +
    'PRAGMA,PRIVATE,PROCEDURE,RAISE,RAISE_APPLICATION_ERROR,RANGE,RE$ATTRIBUTE_VALUE,' +
    'RE$ATTRIBUTE_VALUE_LIST,RE$COLUMN_VALUE,RE$COLUMN_VALUE_LIST,' +
    'RE$NAME_ARRAY,RE$NV_ARRAY,RE$NV_LIST,RE$NV_NODE,RE$RULE_HIT,' +
    'RE$RULE_HIT_LIST,RE$TABLE_ALIAS,RE$TABLE_ALIAS_LIST,' +
    'RE$TABLE_VALUE,RE$TABLE_VALUE_LIST,RE$VARIABLE_TYPE,' +
    'RE$VARIABLE_TYPE_LIST,RE$VARIABLE_VALUE,RE$VARIABLE_VALUE_LIST,RECORD,' +
    'REF,REFERENCING,RELEASE,REMR,REQ,RESP,RESTRICT_REFERENCES,RETURN,' +
    'REVERSE,ROLLBACK,ROWCOUNT,ROWTYPE,RUNTIME_INFO,SAVEPOINT,SCHEMA,' +
    'SELF,SEPARATE,SERIALLY_REUSABLE,SPACE,SQL,SQLERROR,STATEMENT,STRUCT,' +
    'SUBTYPE,TABAUTH,TABLES,TASK,TDO,TERMINATE,TRUE,TYPE,USE,VARYING,VIEWS,' +
    'WHEN,WHILE,WORK,WRITE,XOR';

  // Oracle data types
  OracleTypes: string =
    'ANYDATA,ANYDATASET,ANYTYPE,BFILE,BINARY_DOUBLE,BINARY_FLOAT,BINARY_INTEGER,' +
    'BLOB,BOOLEAN,CHAR,CLOB,DATE,DAY,DBURITYPE,DEC,DECIMAL,DOUBLE,FLOAT,' +
    'HTTPURITYPE,IDENTITY,INTEGER,INTERVAL,LONG,MLSLABEL,MONTH,NATURAL,NATURALN,NCHAR,' +
    'NCLOB,NUMBER,NUMERIC,NVARCHAR2,PLS_INTEGER,POSITIVE,POSITIVEN,RAW,REAL,' +
    'ROWID,SECOND,SMALLINT,TIMESTAMP,URITYPE,UROWID,VARCHAR,VARCHAR2,XDBURITYPE,' +
    'XMLDATA,XMLTYPE,YEAR,ZONE';

  // Oracle built in exceptions
  OracleExceptions: string =
    'ACCESS_INTO_NULL,CASE_NOT_FOUND,COLLECTION_IS_NULL,CURSOR_ALREADY_OPEN,' +
    'DUP_VAL_ON_INDEX,INVALID_CURSOR,INVALID_NUMBER,LOGIN_DENIED,' +
    'NO_DATA_FOUND,NOT_LOGGED_ON,OTHERS,PROGRAM_ERROR,ROWTYPE_MISMATCH,' +
    'SELF_IS_NULL,STORAGE_ERROR,SUBSCRIPT_BEYOND_COUNT,SUBSCRIPT_OUTSIDE_LIMIT,' +
    'SYS_INVALID_ROWID,TIMEOUT_ON_RESOURCE,TOO_MANY_ROWS,VALUE_ERROR,' +
    'ZERO_DIVIDE';

  // Oracle built in functions
  OracleFunctions: string =
    'ABS,ACOS,ADD_MONTHS,AGGREGATE,ANALYTIC,ASCII,ASCIISTR,ASIN,ATAN,ATAN2,' +
    'AVERAGE,AVG,BASE64_DECODE,BASE64_ENCODE,BEGIN_REQUEST,BFILENAME,' +
    'BIN_TO_NUM,BIT_AND,BIT_COMPLEMENT,BIT_OR,BIT_XOR,BITAND,' +
    'CAST_FROM_BINARY_INTEGER,CAST_FROM_NUMBER,CAST_TO_BINARY_INTEGER,' +
    'CAST_TO_NUMBER,CAST_TO_RAW,CAST_TO_VARCHAR2,CEIL,CHARTOROWID,CHR,' +
    'COLUMN_PRESENT,COMPARE,COMPARE_TEMPLATES,COMPOSE,CONCAT,CONVERSION,' +
    'CONVERT,CONVERT_ANYDATA_TO_LCR_DDL,CONVERT_ANYDATA_TO_LCR_ROW,' +
    'COPIES,COPY_TEMPLATE,CORR,COS,COSH,COUNT,COVAR_POP,COVAR_SAMP,' +
    'CREATE_OBJECT_FROM_EXISTING,CREATE_PIPE,CREATE_REFRESH_TEMPLATE,' +
    'CREATE_TEMPLATE_OBJECT,CREATE_TEMPLATE_PARM,CREATE_USER_AUTHORIZATION,' +
    'CREATE_USER_PARM_VALUE,CRLF,CUBE,CUME_DIST,CURRENT_DATE,' +
    'CURRENT_INSTANCE,CURRENT_TIMESTAMP,DATA_BLOCK_ADDRESS_BLOCK,' +
    'DATA_BLOCK_ADDRESS_FILE,DBTIMEZONE,DECODE,DECOMPOSE,DELETE_BREAKPOINT,' +
    'DELETE_OER_BREAKPOINT,DENSE_RANK,DEPTH,DEREF,DISABLE_BREAKPOINT,' +
    'DISABLED,DISPLAY,DROP_ALL,DROP_ELEMENT,DROP_FILE,DUMP,' +
    'EMPTY_BLOB,EMPTY_CLOB,ENABLE_BREAKPOINT,EQUALS_PATH,ESTIMATE_CPU_UNITS,' +
    'EXCLUDE_PUSH,EXECUTE_AND_FETCH,EXECUTE_NON_QUERY,EXISTSNODE,EXP,EXTEND,' +
    'EXTRACT,EXTRACTVALUE,FCOPY,FETCH_ROW,FETCH_ROWS,FGETPOS,FILEEXISTS,' +
    'FILEISOPEN,FIRST,FIRST_VALUE,FLOOR,FLUSH_DATA,FOPEN,FOPEN_NCHAR,' +
    'FORMAT_CALL_STACK,FORMAT_ERROR_STACK,FREMOVE,FRENAME,FROM_REMOTE,FSEEK,' +
    'GET_ARG_FORM,GET_ARG_TYPE,GET_COOKIE_COUNT,GET_COOKIES,' +
    'GET_DETAILED_SQLCODE,GET_DETAILED_SQLERRM,GET_ERROR_MESSAGE,' +
    'GET_HASH_VALUE,GET_HEADER_COUNT,GET_INDEXES,GET_INFORMATION,' +
    'GET_OBJECT_NULL_VECTOR_ARG,GET_PARAMETER_VALUE,' +
    'GET_PERSISTENT_CONN_COUNT,GET_RAW,GET_RESPONSE,GET_RUNTIME_INFO,' +
    'GET_RUNTIME_PARM_ID,GET_SESSION_TIMEOUT,GET_SYSTEM_CHANGE_NUMBER,' +
    'GET_TAG,GET_TIME,GET_TIMEOUT,GET_TIMEOUT_BEHAVIOR,GET_VALUE,' +
    'GETCHUNKSIZE,GETLENGTH,GLB,GREATEST,GREATEST_LB,GROUP_ID,GROUPING,' +
    'GROUPING_ID,HEXTORAW,I_AM_A_REFRESH,INITCAP,INITIALIZE,' +
    'INSTANTIATE_OFFLINE,INSTANTIATE_ONLINE,INSTR,INSTRB,' +
    'INTERNAL_VERSION_CHECK,IS_CLUSTER_DATABASE,IS_LOCATOR,IS_OPEN,' +
    'IS_ROLE_ENABLED,IS_SESSION_ALIVE,IS_TRIGGER_FIRE_ONCE,ISTEMPORARY,LAG,' +
    'LAST,LAST_DAY,LAST_ERROR_POSITION,LAST_ROW_COUNT,LAST_ROW_ID,' +
    'LAST_SQL__CODE,LAST_VALUE,LEAD,LEAST,LEAST_LB,LENGTH,LENGTHB,LINEAR,LN,' +
    'LOCAL_TRANSACTION_ID,LOCALTIMESTAMP,LOG,LOWER,LPAD,LTRIM,LUB,' +
    'MAKE_DATA_BLOCK_ADDRESS,MAKE_REF,MAP_ALL,MAP_ELEMENT,MAP_FILE,' +
    'MAP_OBJECT,MAX,MIN,MINE_VALUE,MISCELLANEOUS,MOD,MONTHS_BETWEEN,NCHR,' +
    'NEW_TIME,NEXT_DAY,NEXT_ITEM_TYPE,NLS_CHARSET_DECL_LEN,NLS_CHARSET_ID,' +
    'NLS_CHARSET_NAME,NLS_INITCAP,NLS_LOWER,NLS_SORT,NLS_UPPER,NLSSORT,NTILE,' +
    'NULLIF,NUMTODSINTERVAL,NUMTOYMINTERVAL,NVARRAY_FIND_NAME,' +
    'NVARRAY_FIND_NAME_TYPE,NVARRAY_GET,NVARRAY_GET_BOOLEAN,NVARRAY_GET_BYTE,' +
    'NVARRAY_GET_DATE,NVARRAY_GET_DOUBLE,NVARRAY_GET_FLOAT,' +
    'NVARRAY_GET_INTEGER,NVARRAY_GET_LONG,NVARRAY_GET_RAW,NVARRAY_GET_SHORT,' +
    'NVARRAY_GET_TEXT,NVL,NVL2,OBJECT,OPEN_CURSOR,OVER,OVERLAY,PATH,' +
    'PAUSE_PROFILER,PERCENT_RANK,PERCENTILE_CONT,PERCENTILE_DISC,PMARKER,' +
    'PORT_STRING,POWER,PURGE,PUSH,PUT_RAW,QUOTED_PRINTABLE_DECODE,' +
    'QUOTED_PRINTABLE_ENCODE,RANDOM,RANK,RATIO_TO_REPORT,RATION_TO_REPORT,' +
    'RAWTOHEX,RAWTONHEX,RECEIVE_MESSAGE,REFERENCE,REFTOHEX,REGR_AVGX,' +
    'REGR_AVGY,REGR_COUNT,REGR_INTERCEPT,REGR_R2,REGR_SLOPE,REGR_SXX,' +
    'REGR_SXY,REGR_SYY,REGRESSION,REMOVE_PIPE,REPLACE,REPLICATION_IS_ON,' +
    'REQUEST,REQUEST_PIECES,RESTORE,RESUME_PROFILER,RETURNING,ROLLUP,ROUND,' +
    'ROW_NUMBER,ROWID_BLOCK_NUMBER,ROWID_CREATE,ROWID_OBJECT,' +
    'ROWID_RELATIVE_FNO,ROWID_ROW_NUMBER,ROWID_TO_ABSOLUTE_FNO,' +
    'ROWID_TO_EXTENDED,ROWID_TO_RESTRICTED,ROWID_TYPE,ROWID_VERIFY,' +
    'ROWIDTOCHAR,ROWIDTONCHAR,RPAD,RTRIM,SEND_MESSAGE,SESSIONTIMEZONE,' +
    'SET_BREAKPOINT,SET_OER_BREAKPOINT,SET_TIMEOUT,SET_VALUE,SIGN,SIN,SINH,' +
    'SOUNDEX,SPACE_ERROR_INFO,SQLCODE,SQLERRM,SQRT,START_PROFILER,STDDEV,' +
    'STDDEV_POP,STDDEV_SAMP,STDDEVP,STDDEVS,STEP_ID,STOP_PROFILER,SUBSTR,' +
    'SUBSTRB,SUM,SYNCHRONIZE,SYS_CONNECT_BY_PATH,SYS_CONTEXT,SYS_DBURIGEN,' +
    'SYS_EXTRACT_UTC,SYS_GUID,SYS_TYPEID,SYS_XMLAGG,SYS_XMLGEN,SYSDATE,' +
    'SYSTIMESTAMP,TAN,TANH,TO_CHAR,TO_CLOB,TO_DATE,TO_DSINTERVAL,TO_LABEL,' +
    'TO_LOB,TO_MULTI_BYTE,TO_NCHAR,TO_NCLOB,TO_NUMBER,TO_SINGLE_BYTE,' +
    'TO_TIMESTAMP,TO_TIMESTAMP_TZ,TO_YMINTERVAL,TRANSLATE,TRANSLITERATE,' +
    'TREAT,TRIM,TRUNC,TZ_OFFSET,UID,UNDER_PATH,UNESCAPE,UNIQUE_SESSION_ID,' +
    'UNIQUE_SESSION_NAME,UNISTR,UPDATEXML,UPPER,USER,USERENV,USING,UUDECODE,' +
    'UUENCODE,VALUE,VAR_POP,VAR_SAMP,VARIANCE,VARP,VARS,VSIZE,WIDTH_BUCKET,' +
    'XMLAGG,XMLCOLATTVAL,XMLCONCAT,XMLELEMENT,XMLFOREST,XMLSEQUENCE,' +
    'XMLTRANSFORM,XRANGE';

  OracleDefaultPackages: string =
    'CTX_ADM,CTX_CLS,CTX_DDL,CTX_DOC,CTX_OUTPUT,CTX_QUERY,CTX_REPORT,CTX_THES,CTX_ULEXER,' +
    'DBMS_ADVANCED_REWRITE,DBMS_ADVISOR,DBMS_ALERT,DBMS_APPLICATION_INFO,' +
    'DBMS_APPLY_ADM,DBMS_AQ,DBMS_AQ_EXP_HISTORY_TABLES,DBMS_AQ_EXP_INDEX_TABLES,' +
    'DBMS_AQ_EXP_QUEUE_TABLES,DBMS_AQ_EXP_QUEUES,' +
    'DBMS_AQ_EXP_SUBSCRIBER_TABLES,DBMS_AQ_EXP_TIMEMGR_TABLES,' +
    'DBMS_AQ_EXP_ZECURITY,DBMS_AQ_IMP_INTERNAL,DBMS_AQ_IMP_ZECURITY,' +
    'DBMS_AQ_IMPORT_INTERNAL,DBMS_AQ_SYS_EXP_ACTIONS,' +
    'DBMS_AQ_SYS_EXP_INTERNAL,DBMS_AQ_SYS_IMP_INTERNAL,DBMS_AQADM,' +
    'DBMS_AQADM_SYS,DBMS_AQADM_SYSCALLS,DBMS_AQELM,DBMS_AQIN,' +
    'DBMS_AQJMS,DBMS_ASSERT,DBMS_AUTO_TASK_ADMIN,DBMS_AW_STATS,DBMS_BACKUP_RESTORE,' +
    'DBMS_CAPTURE_ADM,DBMS_CDC_PUBLISH,DBMS_CDC_SUBSCRIBE,DBMS_COMPARISON,' +
    'DBMS_CONNECTION_POOL,DBMS_CQ_NOTIFICATION,DBMS_CRYPTO,DBMS_CSX_ADMIN,' +
    'DBMS_CUBE,DBMS_CUBE_ADVISE,DBMS_DATA_MINING,DBMS_DATA_MINING_TRANSFORM,' +
    'DBMS_DATAPUMP,DBMS_DB_VERSION,DBMS_DDL,' +
    'DBMS_DEBUG,DBMS_DEFER,DBMS_DEFER_IMPORT_INTERNAL,DBMS_DEFER_QUERY,' +
    'DBMS_DEFER_SYS,DBMS_DESCRIBE,DBMS_DG,DBMS_DIMENSION,DBMS_DISTRIBUTED_TRUST_ADMIN,' +
    'DBMS_EPG,DBMS_ERRLOG,DBMS_EXPFIL,DBMS_EXPORT_EXTENSION,DBMS_FGA,DBMS_FILE_GROUP,' +
    'DBMS_FILE_TRANSFER,DBMS_FLASHBACK,DBMS_FREQUENT_ITEMSET,DBMS_HM,DBMS_HPROF,' +
    'DBMS_HS_PARALLEL,DBMS_HS_PASSTHROUGH,DBMS_IJOB,DBMS_INTERNAL_TRIGGER,DBMS_IOT,' +
    'DBMS_IREFRESH,DBMS_ISNAPSHOT,DBMS_JAVA,DBMS_JAVA_TEST,DBMS_JOB,DBMS_LDAP,DBMS_LDAP_UTL,' +
    'DBMS_LIBCACHE,DBMS_LOB,DBMS_LOCK,DBMS_LOGMNR,DBMS_LOGMNR_CDC_PUBLISH,' +
    'DBMS_LOGMNR_CDC_SUBSCRIBE,DBMS_LOGMNR_D,DBMS_LOGSTDBY,DBMS_METADATA,' +
    'DBMS_MGD_ID_UTL,DBMS_MGWADM,DBMS_MGWMSG,DBMS_MONITOR,DBMS_MVIEW,' +
    'DBMS_NETWORK_ACL_ADMIN,DBMS_NETWORK_UTL, DBMS_OBFUSCATION_TOOLKIT,DBMS_ODCI,' +
    'DBMS_OFFLINE_OG,DBMS_OFFLINE_SNAPSHOT,DBMS_OLAP,' +
    'DBMS_ORACLE_TRACE_AGENT,DBMS_ORACLE_TRACE_USER,DBMS_OUTLN,' +
    'DBMS_OUTLN_EDIT,DBMS_OUTPUT,DBMS_PCLXUTIL,DBMS_PICKLER,DBMS_PIPE,' +
    'DBMS_PITR,DBMS_PLUGTS,DBMS_PROFILER,DBMS_PROPAGATION_ADM,' +
    'DBMS_PRVTAQIM,DBMS_PRVTAQIP,DBMS_PRVTAQIS,DBMS_PRVTRMIE,DBMS_PSP,' +
    'DBMS_PSWMG_IMPORT,DBMS_PREDICTIVE_ANALYTICS,DBMS_PREPROCESSOR,' +
    'DBMS_RANDOM,DBMS_RCVMAN,DBMS_RECTIFIER_DIFF,DBMS_REDEFINITION,DBMS_REFRESH,' +
    'DBMS_REFRESH_EXP_LWM,DBMS_REFRESH_EXP_SITES,DBMS_REPAIR,DBMS_REPCAT,DBMS_REPCAT_ADMIN,' +
    'DBMS_REPCAT_AUTH,DBMS_REPCAT_INSTANTIATE,DBMS_REPCAT_RGT,DBMS_REPUTIL,' +
    'DBMS_RESCONFIG,DBMS_RESOURCE_MANAGER,DBMS_RESOURCE_MANAGER_PRIVS,DBMS_RESULT_CACHE,' +
    'DBMS_RESUMABLE,DBMS_RLMGR,DBMS_RLS,DBMS_RMGR_GROUP_EXPORT,DBMS_RMGR_PACT_EXPORT,' +
    'DBMS_RMGR_PLAN_EXPORT,DBMS_RMIN,DBMS_ROWID,DBMS_RULE,DBMS_RULE_ADM,' +
    'DBMS_RULE_EXIMP,DBMS_SCHEDULER,DBMS_SERVER_ALERT,DBMS_SERVICE,DBMS_SESSION,' +
    'DBMS_SHARED_POOL,DBMS_SNAP_INTERNAL,DBMS_SNAP_REPAPI,DBMS_SNAPSHOT,DBMS_SNAPSHOT_UTL,' +
    'DBMS_SPACE,DBMS_SPACE_ADMIN,DBMS_SQL,DBMS_SQLDIAG,DBMS_SQLTUNE,DBMS_STANDARD,' +
    'DBMS_STAT_FUNCS,DBMS_STATS,DBMS_STORAGE_MAP,' +
    'DBMS_STREAMS,DBMS_STREAMS_ADM,DBMS_STREAMS_AUTH,DBMS_STREAMS_ADVISOR_ADM,' +
    'DBMS_STREAMS_MESSAGING,DBMS_STREAMS_TABLESPACE_ADM,DBMS_SUMADV,DBMS_SUMMARY,' +
    'DBMS_SUMREF_CHILD,DBMS_SUMREF_PARENT,DBMS_SUMREF_UTIL,' +
    'DBMS_SUMREF_UTIL2,DBMS_SUMVDM,DBMS_SYS_ERROR,DBMS_SYS_SQL,' +
    'DBMS_SYSTEM,DBMS_TDB,DBMS_TRACE,DBMS_TRANSACTION,DBMS_TRANSFORM,DBMS_TTS,' +
    'DBMS_TYPES,DBMS_UTILITY,DBMS_WORKLOAD_CAPTURE,DBMS_WORKLOAD_REPLAY,' +
    'DBMS_WORKLOAD_REPOSITORY,DBMS_WM,DBMS_XA,DBMS_XDB,DBMS_XDB_ADMIN,' +
    'DBMS_XDB_VERSION,DBMS_XDBRESOURCE,DBMS_XDBT,DBMS_XDBZ,DBMS_XEVENT,' +
    'DBMS_XMLDOM,DBMS_XMLGEN,DBMS_XMLINDEX,DBMS_XMLPARSER,DBMS_XMLQUERY,' +
    'DBMS_XMLSAVE,DBMS_XMLSCHEMA,DBMS_XMLSTORE,DBMS_XMLTRANSLATIONS,DBMS_XPLAN,' +
    'DBMS_XSLPROCESSOR,DBMS_ZHELP,DBMS_ZHELP_IR,DBMSZEXP_SYSPKGGRNT,DEBUG_EXTPROC,' +
    'DIANA,DIUTIL,HTF,HTP,ODCICONST,OUTLN_PKG,PBREAK,PBRPH,PBSDE,PBUTL,PIDL,PLITBLM,' +
    'SDO_CS,SDO_GEOM,SDO_LRS,SDO_MIGRATE,SDO_TUNE,SDO_UTIL,STANDARD,' +
    'SYS_STUB_FOR_PURITY_ANALYSIS,UTL_COLL,UTL_ENCODE,UTL_FILE,UTL_FILE_DIR,UTL_HTTP,' +
    'UTL_I18N,UTL_INADDR,UTL_LMS,UTL_MAIL,UTL_NLA,UTL_PG,UTL_RAW,UTL_REF,UTL_SMTP,' +
    'UTL_SPADV,UTL_TCP,UTL_URL,WPG_DOCLOAD';

  OracleSQLPlusCommands: string =
    'APP,APPINFO,AQ$_AGENT,AQ$_AGENT_LIST_T,AQ$_DESCRIPTOR,AQ$_POST_INFO,' +
    'AQ$_POST_INFO_LIST,AQ$_RECIPIENT_LIST_T,AQ$_REG_INFO,AQ$_REG_INFO_LIST,' +
    'AQ$_SUBSCRIBER_LIST_T,ARCHIVE,ARRAYSIZE,ATTRIBUTE,AUTOCOMMIT,AUTOP,' +
    'AUTOPRINT,AUTORECOVERY,AUTOT,AUTOTRACE,BLO,BLOCKTERMINATOR,BRE,BREAK,' +
    'BTI,BTITLE,BUFFER,CL,CLEAR,CLOSECURSOR,CMDS,CMDSEP,COL,COLSEP,COM,COMP,' +
    'COMPAT,COMPATIBILITY,CON,CONN,COPY,COPYC,COPYCOMMIT,COPYTYPECHECK,DEF,' +
    'DEFINE,DESC,DESCR,DESCRI,DESCRIB,DESCRIBE,DISC,DISCO,DISCON,DISCONN,' +
    'DISCONNE,DISCONNEC,DISCONNECT,EA,ECHO,EDITF,EDITFILE,EMB,' +
    'EMBEDDED,ESC,EXEC,EXECUTE,FAILURE,FEED,FEEDBACK,FLAGGER,FLU,GET,' +
    'HEA,HEADING,HEADS,HEADSEP,HELP,HO,HOST,INPUT,INTERMED,INTERMEDIATE,INV,' +
    'INVISIBLE,LIN,LINESIZE,LO,LOBOF,LOBOFFSET,LOGON,LOGSOURCE,LONGC,' +
    'LONGCHUNKSIZE,MARKUP,MAXDATA,MIX,MIXED,NATIVE,NEWP,NEWPAGE,NUM,' +
    'NUMF,NUMFORMAT,NUMWIDTH,OFF,OSERROR,PAGES,PAGESIZE,PASSW,PAU,PAUSE,' +
    'PPRINT,PRI,PRINT,RECSEP,RECSEPCHAR,REPF,REPFOOTER,REPH,REPHEADER,' +
    'RUN,SAVE,SCAN,SERVEROUTPUT,SET,SHIFT,SHIFTINOUT,SHO,SHOW,SHUTDOWN,' +
    'SILENT,SPOOL,SQLBL,SQLBLANKLINES,SQLC,SQLCASE,SQLCO,SQLCONTINUE,SQLN,' +
    'SQLNUMBER,SQLP,SQLPRE,SQLPREFIX,SQLPROMPT,SQLT,SQLTERMINATOR,STA,' +
    'STARTUP,STATEMENT_ID,STORE,SUCCESS,SUF,SUFFIX,TAB,TERM,TERMOUT,TI,TIMI,' +
    'TIMING,TRIMOUT,TRIMS,TRIMSPOOL,TTI,TTITLE,UND,UNDEF,UNDEFINE,' +
    'UNDERLINE,UP,VAR,VARIABLE,VER,VERIFY,VERSION,VIS,VISIBLE,WHENEVER,WR,' +
    'WRA,WRAP,WRAPPED';

  OracleCommentKW: string =
    'REM,REMA,REMAR,REMARK';

  OracleConsoleOutputKW: string =
    'PRO,PROM,PROMP,PROMPT';

//---MS-SQL 7-------------------------------------------------------------------
  // keywords
  MSSQL7KW: string =
    'ABSOLUTE,ADD,ALL,ALTER,ANY,AS,ASC,AUTHORIZATION,AVG,BACKUP,BEGIN,' +
    'BETWEEN,BREAK,BROWSE,BULK,BY,CASCADE,CHECK,CHECKPOINT,CLOSE,CLUSTERED,' +
    'COLUMN,COMMIT,COMMITTED,COMPUTE,CONFIRM,CONSTRAINT,CONTAINS,' +
    'CONTAINSTABLE,CONTINUE,CONTROLROW,COUNT,CREATE,CROSS,CURRENT,' +
    'CURRENT_DATE,CURRENT_TIME,CURSOR,DATABASE,DBCC,DEALLOCATE,DECLARE,' +
    'DEFAULT,DELETE,DENY,DESC,DISK,DISTINCT,DISTRIBUTED,DOUBLE,DROP,DUMMY,' +
    'DUMP,ELSE,END,ERRLVL,ERROREXIT,ESCAPE,EXCEPT,EXEC,EXECUTE,EXISTS,EXIT,' +
    'FETCH,FILE,FILLFACTOR,FIRST,FLOPPY,FOR,FOREIGN,FREETEXT,FREETEXTTABLE,' +
    'FROM,FULL,GLOBAL,GOTO,GRANT,GROUP,HAVING,HOLDLOCK,IDENTITY,IDENTITYCOL,' +
    'IDENTITY_INSERT,IF,IN,INDEX,INNER,INSERT,INTERSECT,INTO,IS,ISOLATION,' +
    'JOIN,KEY,KILL,LAST,LEFT,LEVEL,LIKE,LINENO,LOAD,MAX,MIN,MIRROREXIT,' +
    'NATIONAL,NEXT,NOCHECK,NONCLUSTERED,NOT,NULL,OF,OFF,OFFSETS,ON,ONCE,' +
    'ONLY,OPEN,OPENDATASOURCE,OPENQUERY,OPENROWSET,OPTION,OR,ORDER,OUTER,' +
    'OVER,PERCENT,PERM,PERMANENT,PIPE,PLAN,PRECISION,PREPARE,PRIMARY,PRINT,' +
    'PRIOR,PRIVILEGES,PROC,PROCEDURE,PROCESSEXIT,PUBLIC,RAISERROR,READ,' +
    'READTEXT,RECONFIGURE,REFERENCES,RELATIVE,REPEATABLE,REPLICATION,RESTORE,' +
    'RESTRICT,RETURN,REVOKE,RIGHT,ROLLBACK,ROWCOUNT,ROWGUIDCOL,RULE,SAVE,' +
    'SCHEMA,SELECT,SERIALIZABLE,SET,SETUSER,SHUTDOWN,SOME,STATISTICS,SUM,' +
    'TABLE,TAPE,TEMP,TEMPORARY,TEXTSIZE,THEN,TO,TOP,TRAN,TRANSACTION,TRIGGER,' +
    'TRUNCATE,TSEQUAL,UNCOMMITTED,UNION,UNIQUE,UPDATE,UPDATETEXT,USE,USER,' +
    'VALUES,VARYING,VIEW,WAITFOR,WHEN,WHERE,WHILE,WITH,WORK,WRITETEXT';

  // functions
  MSSQL7Functions: string =
    '@@CONNECTIONS,@@CPU_BUSY,@@CURSOR_ROWS,@@DATEFIRST,@@DBTS,@@ERROR,' +
    '@@FETCH_STATUS,@@IDENTITY,@@IDLE,@@IO_BUSY,@@LANGID,@@LANGUAGE,' +
    '@@LOCK_TIMEOUT,@@MAX_CONNECTIONS,@@MAX_PRECISION,@@NESTLEVEL,@@OPTIONS,' +
    '@@PACKET_ERRORS,@@PACK_RECEIVED,@@PACK_SENT,@@PROCID,@@REMSERVER,' +
    '@@ROWCOUNT,@@SERVERNAME,@@SERVICENAME,@@SPID,@@TEXTSIZE,@@TIMETICKS,' +
    '@@TOTAL_ERRORS,@@TOTAL_READ,@@TOTAL_WRITE,@@TRANCOUNT,@@VERSION,ABS,' +
    'ACOS,AND,APP_NAME,ASCII,ASIN,ATAN,ATN2,CASE,CAST,CEILING,CHARINDEX,' +
    'COALESCE,COLUMNPROPERTY,COL_LENGTH,COL_NAME,CONVERT,COS,COT,' +
    'CURRENT_TIMESTAMP,CURRENT_USER,CURSOR_STATUS,DATABASEPROPERTY,' +
    'DATALENGTH,DATEADD,DATEDIFF,DATENAME,DATEPART,DAY,DB_ID,DB_NAME,' +
    'DEGREES,DIFFERENCE,EXP,FILEGROUPPROPERTY,FILEGROUP_ID,FILEGROUP_NAME,' +
    'FILEPROPERTY,FILE_ID,FILE_NAME,FLOOR,FORMATMESSAGE,' +
    'FULLTEXTCATALOGPROPERTY,FULLTEXTSERVICEPROPERTY,GETANSINULL,GETDATE,' +
    'HOST_ID,HOST_NAME,IDENT_INCR,IDENT_SEED,INDEXPROPERTY,INDEX_COL,' +
    'ISDATE,ISNULL,ISNUMERIC,IS_MEMBER,IS_SRVROLEMEMBER,LEN,LOG,LOG10,LOWER,' +
    'LTRIM,MONTH,NEWID,NULLIF,OBJECTPROPERTY,OBJECT_ID,OBJECT_NAME,PARSENAME,' +
    'PATINDEX,PERMISSIONS,PI,POWER,QUOTENAME,RADIANS,RAND,REPLACE,REPLICATE,' +
    'REVERSE,ROUND,RTRIM,SESSION_USER,SIGN,SIN,SOUNDEX,SPACE,SQRT,SQUARE,' +
    'STATS_DATE,STR,STUFF,SUBSTRING,SUSER_ID,SUSER_NAME,SUSER_SID,' +
    'SUSER_SNAME,SYSTEM_USER,TAN,TEXTPTR,TEXTVALID,TYPEPROPERTY,UNICODE,' +
    'UPPER,USER_ID,USER_NAME,YEAR';

  // types
  MSSQL7Types: string =
    'BINARY,BIT,CHAR,DATETIME,DECIMAL,FLOAT,IMAGE,INT,MONEY,NCHAR,NTEXT,' +
    'NUMERIC,NVARCHAR,REAL,SMALLDATETIME,SMALLINT,SMALLMONEY,SYSNAME,TEXT,' +
    'TIMESTAMP,TINYINT,UNIQUEIDENTIFIER,VARBINARY,VARCHAR';

//---MS-SQL2K-------------------------------------------------------------------
  // keywords
  MSSQL2000KW: string =
    'ADD,ALL,ALTER,AND,ANY,AS,ASC,AUTHORIZATION,BACKUP,' +
    'BEGIN,BETWEEN,BREAK,BROWSE,BULK,BY,CASCADE,CASE,' +
    'CHECK,CHECKPOINT,CLOSE,CLUSTERED,COLLATE,' +
    'COLUMN,COMMIT,COMPUTE,CONSTRAINT,CONTAINS,CONTAINSTABLE,' +
    'CONTINUE,CREATE,CROSS,CURRENT,CURSOR,DATABASE,' +
    'DBCC,DEALLOCATE,DECLARE,DEFAULT,DELETE,DENY,DESC,DISK,' +
    'DISTINCT,DISTRIBUTED,DOUBLE,DROP,DUMMY,DUMP,ELSE,END,' +
    'ERRLVL,ESCAPE,EXCEPT,EXEC,EXECUTE,EXISTS,EXIT,FETCH,FILE,' +
    'FILLFACTOR,FOR,FOREIGN,FORMSOF,FREETEXT,FREETEXTTABLE,FROM,FULL,' +
    'FUNCTION,GOTO,GRANT,GROUP,HAVING,HOLDLOCK,IDENTITY,' +
    'IDENTITYCOL,IDENTITY_INSERT,IF,IN,INFLECTIONAL,INDEX,INNER,INSERT,' +
    'INTERSECT,INTO,IS,ISABOUT,JOIN,KEY,KILL,LEFT,LIKE,LINENO,LOAD,' +
    'NATIONAL,NOCHECK,NONCLUSTERED,NOT,NULL,NULLIF,OF,OFF,' +
    'OFFSETS,ON,OPEN,OPENDATASOURCE,OPENQUERY,OPENROWSET,OPENXML,' +
    'OPTION,OR,ORDER,OUTER,OVER,PERCENT,PLAN,PRECISION,' +
    'PRIMARY,PRINT,PROC,PROCEDURE,PUBLIC,RAISERROR,READ,' +
    'READTEXT,RECONFIGURE,REFERENCES,REPLICATION,RESTORE,' +
    'RESTRICT,RETURN,REVOKE,RIGHT,ROLLBACK,ROWCOUNT,ROWGUIDCOL,' +
    'RULE,SAVE,SCHEMA,SELECT,SESSION_USER,SET,SETUSER,SHUTDOWN,' +
    'SOME,STATISTICS,TABLE,TEXTSIZE,THEN,TO,TOP,TRAN,TRANSACTION,' +
    'TRIGGER,TRUNCATE,TSEQUAL,UNION,UNIQUE,UPDATE,UPDATETEXT,' +
    'USE,USER,VALUES,VARYING,VIEW,WAITFOR,WEIGHT,WHEN,WHERE,WHILE,' +
    'WITH,WRITETEXT';

  // functions
  MSSQL2000Functions: string =
    '@@CONNECTIONS,@@CPU_BUSY,@@CURSOR_ROWS,@@DATEFIRST,@@DBTS,@@ERROR,' +
    '@@FETCH_STATUS,@@IDENTITY,@@IDLE,@@IO_BUSY,@@LANGID,@@LANGUAGE,' +
    '@@LOCK_TIMEOUT,@@MAX_CONNECTIONS,@@MAX_PRECISION,@@NESTLEVEL,@@OPTIONS,' +
    '@@PACKET_ERRORS,@@PACK_RECEIVED,@@PACK_SENT,@@PROCID,@@REMSERVER,' +
    '@@ROWCOUNT,@@SERVERNAME,@@SERVICENAME,@@SPID,@@TEXTSIZE,@@TIMETICKS,' +
    '@@TOTAL_ERRORS,@@TOTAL_READ,@@TOTAL_WRITE,@@TRANCOUNT,@@VERSION,' +
    'ABS,ACOS,APP_NAME,ASCII,ASIN,ATAN,ATN2,AVG,BINARY_CHECKSUM,CAST,' +
    'CEILING,CHARINDEX,CHECKSUM,CHECKSUM_AGG,COALESCE,COLLATIONPROPERTY,' +
    'COLUMNPROPERTY,COL_LENGTH,COL_NAME,CONVERT,COS,COT,COUNT,' +
    'COUNT_BIG,CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,' +
    'CURRENT_USER,CURSOR_STATUS,DATABASEPROPERTY,DATABASEPROPERTYEX,' +
    'DATALENGTH,DATEADD,DATEDIFF,DATENAME,DATEPART,DAY,DB_ID,DB_NAME,DEGREES,' +
    'DIFFERENCE,EXP,FILEGROUPPROPERTY,FILEGROUP_ID,FILEGROUP_NAME,' +
    'FILEPROPERTY,FILE_ID,FILE_NAME,FLOOR,fn_helpcollations,' +
    'fn_listextendedproperty,fn_servershareddrives,fn_trace_geteventinfo,' +
    'fn_trace_getfilterinfo,fn_trace_getinfo,fn_trace_gettable,' +
    'fn_virtualfilestats,FORMATMESSAGE,FULLTEXTCATALOGPROPERTY,' +
    'FULLTEXTSERVICEPROPERTY,GETANSINULL,GETDATE,GETUTCDATE,GROUPING,' +
    'HAS_DBACCESS,HOST_ID,HOST_NAME,IDENT_CURRENT,IDENT_INCR,IDENT_SEED,' +
    'INDEXKEY_PROPERTY,INDEXPROPERTY,INDEX_COL,ISDATE,ISNULL,ISNUMERIC,' +
    'IS_MEMBER,IS_SRVROLEMEMBER,LEN,LOG,LOG10,LOWER,LTRIM,MAX,MIN,MONTH,' +
    'NEWID,OBJECTPROPERTY,OBJECT_ID,OBJECT_NAME,PARSENAME,PATINDEX,' +
    'PERMISSIONS,PI,POWER,QUOTENAME,RADIANS,RAND,REPLACE,REPLICATE,REVERSE,' +
    'ROUND,ROWCOUNT_BIG,RTRIM,SCOPE_IDENTITY,SERVERPROPERTY,SESSIONPROPERTY,' +
    'SIGN,SIN,SOUNDEX,SPACE,SQL_VARIANT_PROPERTY,SQRT,SQUARE,' +
    'STATS_DATE,STDEV,STDEVP,STR,STUFF,SUBSTRING,SUM,SUSER_SID,SUSER_SNAME,' +
    'SYSTEM_USER,TAN,TEXTPTR,TEXTVALID,TYPEPROPERTY,UNICODE,UPPER,' +
    'USER_ID,USER_NAME,VAR,VARP,YEAR';

  // types
  MSSQL2000Types: string =
    'bigint,binary,bit,char,character,datetime,' +
    'dec,decimal,float,image,int,' +
    'integer,money,nchar,ntext,numeric,nvarchar,real,' +
    'rowversion,smalldatetime,smallint,smallmoney,' +
    'sql_variant,sysname,text,timestamp,tinyint,uniqueidentifier,' +
    'varbinary,varchar';

//---Interbase 6----------------------------------------------------------------
  // functions
  Interbase6Functions: string = 'AVG,CAST,COUNT,GEN_ID,MAX,MIN,SUM,UPPER';

  // keywords
  Interbase6KW: string = 'ACTIVE,ADD,AFTER,ALL,ALTER,AND,ANY,AS,ASC,' +
    'ASCENDING,AT,AUTO,AUTODDL,BASED,BASENAME,BASE_NAME,BEFORE,BEGIN,BETWEEN,' +
    'BLOBEDIT,BUFFER,BY,CACHE,CHARACTER_LENGTH,CHAR_LENGTH,CHECK,' +
    'CHECK_POINT_LEN,CHECK_POINT_LENGTH,COLLATE,COLLATION,COLUMN,COMMIT,' +
    'COMMITED,COMPILETIME,COMPUTED,CLOSE,CONDITIONAL,CONNECT,CONSTRAINT,' +
    'CONTAINING,CONTINUE,CREATE,CURRENT,CURRENT_DATE,CURRENT_TIME,' +
    'CURRENT_TIMESTAMP,CURSOR,DATABASE,DAY,DB_KEY,DEBUG,DEC,DECLARE,DEFAULT,' +
    'DELETE,DESC,DESCENDING,DESCRIBE,DESCRIPTOR,DISCONNECT,DISTINCT,DO,' +
    'DOMAIN,DROP,ECHO,EDIT,ELSE,END,ENTRY_POINT,ESCAPE,EVENT,EXCEPTION,' +
    'EXECUTE,EXISTS,EXIT,EXTERN,EXTERNAL,EXTRACT,FETCH,FILE,FILTER,FOR,' +
    'FOREIGN,FOUND,FROM,FULL,FUNCTION,GDSCODE,GENERATOR,GLOBAL,GOTO,GRANT,' +
    'GROUP,GROUP_COMMIT_WAIT,GROUP_COMMIT_WAIT_TIME,HAVING,HELP,HOUR,IF,' +
    'IMMEDIATE,IN,INACTIVE,INDEX,INDICATOR,INIT,INNER,INPUT,INPUT_TYPE,' +
    'INSERT,INT,INTO,IS,ISOLATION,ISQL,JOIN,KEY,LC_MESSAGES,LC_TYPE,LEFT,' +
    'LENGTH,LEV,LEVEL,LIKE,LOGFILE,LOG_BUFFER_SIZE,LOG_BUF_SIZE,LONG,MANUAL,' +
    'MAXIMUM,MAXIMUM_SEGMENT,MAX_SEGMENT,MERGE,MESSAGE,MINIMUM,MINUTE,' +
    'MODULE_NAME,MONTH,NAMES,NATIONAL,NATURAL,NCHAR,NO,NOAUTO,NOT,NULL,' +
    'NUM_LOG_BUFFS,NUM_LOG_BUFFERS,OCTET_LENGTH,OF,ON,ONLY,OPEN,OPTION,OR,' +
    'ORDER,OUTER,OUTPUT,OUTPUT_TYPE,OVERFLOW,PAGE,PAGELENGTH,PAGES,PAGE_SIZE,' +
    'PARAMETER,PASSWORD,PLAN,POSITION,POST_EVENT,PRECISION,PREPARE,PROCEDURE,' +
    'PROTECTED,PRIMARY,PRIVILEGES,PUBLIC,QUIT,RAW_PARTITIONS,READ,REAL,' +
    'RECORD_VERSION,REFERENCES,RELEASE,RESERV,RESERVING,RETAIN,RETURN,' +
    'RETURNING_VALUES,RETURNS,REVOKE,RIGHT,ROLLBACK,RUNTIME,SCHEMA,SECOND,' +
    'SEGMENT,SELECT,SET,SHADOW,SHARED,SHELL,SHOW,SINGULAR,SIZE,SNAPSHOT,SOME,' +
    'SORT,SQL,SQLCODE,SQLERROR,SQLWARNING,STABILITY,STARTING,STARTS,' +
    'STATEMENT,STATIC,STATISTICS,SUB_TYPE,SUSPEND,TABLE,TERMINATOR,THEN,TO,' +
    'TRANSACTION,TRANSLATE,TRANSLATION,TRIGGER,TRIM,TYPE,UNCOMMITTED,UNION,' +
    'UNIQUE,UPDATE,USER,USING,VALUE,VALUES,VARIABLE,VARYING,VERSION,VIEW,' +
    'WAIT,WEEKDAY,WHEN,WHENEVER,WHERE,WHILE,WITH,WORK,WRITE,YEAR,YEARDAY';

  // types
  Interbase6Types: string =
    'BLOB,CHAR,CHARACTER,DATE,DECIMAL,DOUBLE,FLOAT,INTEGER,' +
    'NUMERIC,SMALLINT,TIME,TIMESTAMP,VARCHAR';

//---MySQL----------------------------------------------------------------------
  // keywords
  MySqlKW: string =
    'ACCOUNT,ACTION,ADD,AFTER,AGAINST,ALGORITHM,ALL,ALTER,ANY,ALWAYS,ANALYZE,' +
    'AND,AS,ASC,AT,AUTO_INCREMENT,AVG_ROW_LENGTH,BEFORE,BEGIN,BETWEEN,BINLOG,' +
    'BLOCK,BOTH,BY,CACHE,CALL,CASCADE,CASCADED,CATALOG_NAME,CHANGE,CHANGED,' +
    'CHANNEL,CHAIN,CHARACTER,CHARSET,CHECK,CHECKSUM,CLASS_ORIGIN,CLIENT,CODE,' +
    'COLLATE,COLLATION,COLUMN,COLUMN_FORMAT,COLUMN_NAME,COLUMNS,COMMENT,' +
    'COMMIT,COMMITTED,COMPLETION,CONCURRENT,CONDITION,CONNECTION,CONSISTENT,' +
    'CONSTRAINT,CONSTRAINT_CATALOG,CONSTRAINT_NAME,CONSTRAINT_SCHEMA,' +
    'CONTAINS,CONTEXT,CONTINUE,CONVERT,COPY,CPU,CREATE,CROSS,CURRENT,CURSOR,' +
    'CURSOR_NAME,CYCLE,DATA,DATABASE,DATABASES,DATAFILE,DAY_HOUR,' +
    'DAY_MICROSECOND,DAY_MINUTE,DAY_SECOND,DEALLOCATE,DECLARE,DEFAULT,' +
    'DEFINER,DELAY_KEY_WRITE,DELAYED,DELETE,DESC,DESCRIBE,DETERMINISTIC,' +
    'DIAGNOSTICS,DIRECTORY,DISABLE,DISCARD,DISTINCT,DISTINCTROW,DIV,DO,DROP,' +
    'DUAL,DUMPFILE,DUPLICATE,EACH,ELSE,ELSEIF,ENABLE,ENCLOSED,END,ENDS,' +
    'ENGINE,ENGINES,ERROR,ERRORS,ESCAPE,ESCAPED,EVENT,EVENTS,EVERY,EXCHANGE,' +
    'EXCLUSIVE,EXECUTE,EXISTS,EXPANSION,EXPIRE,EXPLAIN,EXTENDED,FALSE,FAST,' +
    'FAULTS,FILE_BLOCK_SIZE,FLUSH,FIELDS,FILE,FIRST,FOLLOWS,FOR,FORCE,' +
    'FOREIGN,FOUND,FROM,FULL,FULLTEXT,FUNCTION,GENERAL,GENERATED,GET,GLOBAL,' +
    'GRANT,GRANTS,GROUP,HAVING,HELP,HIGH_PRIORITY,HOST,HOSTS,' +
    'HOUR_MICROSECOND,HOUR_MINUTE,HOUR_SECOND,IDENTIFIED,IGNORE,' +
    'IGNORE_SERVER_IDS,IMPORT,IN,INCREMENT,INDEX,INDEXES,INFILE,INITIAL_SIZE,' +
    'INNER,INOUT,INPLACE,INSTANCE,INSERT,INSERT_METHOD,INSTALL,INTO,INVOKER,' +
    'IO,IPC,IS,ISOLATION,JOIN,JSON,KEY,KEY_BLOCK_SIZE,KEYS,KILL,LANGUAGE,' +
    'LAST,LEADING,LESS,LEVEL,LIKE,LIMIT,LINEAR,LINES,LIST,LOAD,LOCAL,LOCK,' +
    'LOGS,LOW_PRIORITY,MASTER,MASTER_AUTO_POSITION,MASTER_BIND,' +
    'MASTER_CONNECT_RETRY,MASTER_DELAY,MASTER_HEARTBEAT_PERIOD,MASTER_HOST,' +
    'MASTER_LOG_FILE,MASTER_LOG_POS,MASTER_PASSWORD,MASTER_PORT,' +
    'MASTER_RETRY_COUNT,MASTER_SSL,MASTER_SSL_CA,MASTER_SSL_CAPATH,' +
    'MASTER_SSL_CERT,MASTER_SSL_CIPHER,MASTER_SSL_CRL,MASTER_SSL_CRLPATH,' +
    'MASTER_SSL_KEY,MASTER_SSL_VERIFY_SERVER_CERT,MASTER_TLS_VERSION,' +
    'MASTER_USER,MATCH,MAX_QUERIES_PER_HOUR,MAX_ROWS,' +
    'MAX_CONNECTIONS_PER_HOUR,MAX_STATEMENT_TIME,MAX_UPDATES_PER_HOUR,' +
    'MAX_USER_CONNECTIONS,MAXVALUE,MEDIUM,MESSAGE_TEXT,MIGRATE,MIN_ROWS,' +
    'MINUTE_MICROSECOND,MINUTE_SECOND,MINVALUE,MOD,MODE,MODIFIES,MODIFY,' +
    'MUTEX,MYSQL_ERRNO,NAME,NAMES,NATURAL,NEVER,NEW,NEXT,NO,NOCACHE,NOCYCLE,' +
    'NOMAXVALUE,NOMINVALUE,NONE,NOT,NULL,NO_WRITE_TO_BINLOG,NUMBER,OFFSET,' +
    'OJ,OLD,ON,ONE,ONLINE,ONLY,OPEN,OPTIMIZE,OPTION,OPTIONALLY,OPTIONS,OR,' +
    'ORDER,OUT,OUTER,OUTFILE,OWNER,PACK_KEYS,PAGE,PAGE_CHECKSUM,PARSER,' +
    'PARTIAL,PARTITION,PARTITIONING,PARTITIONS,PERSIST,PERSISTENT,PHASE,' +
    'PLUGINS,PLUGIN,PORT,PRECEDES,PREPARE,PRESERVE,PRIMARY,PRIVILEGES,' +
    'PROCEDURE,PROCESS,PROCESSLIST,PROFILE,PROFILES,PROXY,PURGE,QUERY,QUICK,' +
    'RANGE,READ,READS,REBUILD,RECOVER,REFERENCES,REGEXP,RELAY,RELAYLOG,' +
    'RELEASE,RELAY_LOG_FILE,RELAY_LOG_POS,RELOAD,REMOVE,RENAME,REORGANIZE,' +
    'REPAIR,REPEATABLE,REPLACE,REPLICATION,REQUIRE,RESET,RESIGNAL,' +
    'RESTRICT,RESUME,RETURN,RETURNED_SQLSTATE,RETURNS,REVOKE,RLIKE,ROLLBACK,' +
    'ROLLUP,ROTATE,ROUTINE,ROW,ROW_FORMAT,ROWS,SAVEPOINT,SCHEDULE,SCHEMA,' +
    'SCHEMA_NAME,SECOND_MICROSECOND,SECURITY,SELECT,SEPARATOR,SEQUENCE,' +
    'SERIALIZABLE,SERVER,SESSION,SET,SHARE,SHARED,SHOW,SHUTDOWN,SIGNAL,' +
    'SIMPLE,SLAVE,SLOW,SNAPSHOT,SOCKET,SOME,SONAME,SOUNDS,SOURCE,SPATIAL,' +
    'SQL,SQL_BIG_RESULT,SQL_BUFFER_RESULT,SQL_CACHE,SQL_CALC_FOUND_ROWS,' +
    'SQL_NO_CACHE,SQL_SMALL_RESULT,SQL_TSI_DAY,SQL_TSI_HOUR,' +
    'SQL_TSI_MICROSECOND,SQL_TSI_MINUTE,SQL_TSI_MONTH,SQL_TSI_QUARTER,' +
    'SQL_TSI_SECOND,SQL_TSI_WEEK,SQL_TSI_YEAR,SQLEXCEPTION,SQLSTATE,' +
    'SQLWARNING,STACKED,STARTING,START,STARTS,STATS_AUTO_RECALC,' +
    'STATS_PERSISTENT,STATS_SAMPLE_PAGES,STATUS,STOP,STORAGE,STORED,' +
    'STRAIGHT_JOIN,SUBCLASS_ORIGIN,SUBPARTITION,SUBPARTITIONS,SUPER,SUSPEND,' +
    'SWAPS,SWITCHES,TABLE,TABLE_CHECKSUM,TABLE_NAME,TABLES,TABLESPACE,' +
    'TEMPORARY,TERMINATED,THAN,TO,TRADITIONAL,TRAILING,TRANSACTION,' +
    'TRANSACTIONAL,TRIGGER,TRIGGERS,TRUE,TYPE,UNCOMMITTED,UNDO,UNINSTALL,' +
    'UNION,UNIQUE,UNKNOWN,UNLOCK,UPDATE,UPGRADE,USAGE,USE,USE_FRM,' +
    'USING,VALIDATION,VALUE,VALUES,VARIABLES,VIEW,VIRTUAL,WAIT,WARNINGS,' +
    'WHERE,WRAPPER,WRITE,WITH,WITHOUT,WORK,XA,XID,XML,XOR,YEAR_MONTH';

  // PLSQL keywords
  MySQLPLSQLKW: string =
    'CASE,CLOSE,EXIT,FETCH,GOTO,HANDLER,ITERATE,IF,LEAVE,LOOP,REPEAT,THEN,' +
    'UNTIL,WHEN,WHILE';

  MySQLTypes: string =

    // Table Engines
    'ARCHIVE,BDB,BERKELEYDB,BLACKHOLE,CSV,EXAMPLE,FEDERATED,HEAP,INNOBASE,' +
    'InnoDB,ISAM,MEMORY,MERGE,MRG_ISAM,MRG_MYISAM,MyISAM,NDB,NDBCLUSTER,' +

    // Index Types
    'BTREE,HASH,' +

    // Column Types
    'bigint,blob,bool,boolean,byte,char,date,datetime,decimal,double,enum,' +
    'float,geometry,geometrycollection,int,integer,linestring,longblob,' +
    'longtext,mediumblob,mediumint,mediumtext,multilinestring,multipoint,' +
    'multipolygon,national,numeric,point,polygon,precision,real,serial,' +
    'signed,smallint,string,text,time,timestamp,tinyblob,tinyint,tinytext,' +
    'unicode,unsigned,varbinary,varchar,year,zerofill,' +

    // Row Formats
    'COMPACT,COMPRESSED,DISK,DYNAMIC,FIXED,REDUNDANT,' +

    // Raid Types
    'RAID0,STRIPED,' +

    // View Algorythm
    'UNDEFINED,TEMPTABLE,' +

    // Charsets
    'armscii8,big5,cp1250,cp1251,cp1256,cp1257,cp850,cp852,' +
    'cp866,cp932,dec8,eucjpms,euckr,gb18030,gb2312,gbk,geostd8,greek,hebrew,' +
    'hp8,keybcs2,koi8r,koi8u,latin1,latin2,latin5,latin7,macce,macroman,sjis,' +
    'swe7,tis620,ucs2,ujis,utf16,utf16le,utf32,utf8,utf8mb4,' +

    '_armscii8,_big5,_cp1250,_cp1251,_cp1256,_cp1257,_cp850,' +
    '_cp852,_cp866,_cp932,_dec8,_eucjpms,_euckr,_gb18030,_gb2312,_gbk,' +
    '_geostd8,_greek,_hebrew,_hp8,_keybcs2,_koi8r,_koi8u,_latin1,_latin2,' +
    '_latin5,_latin7,_macce,_macroman,_sjis,_swe7,_tis620,_ucs2,_ujis,_utf16,' +
    '_utf16le,_utf32,_utf8,_utf8mb4,' +

    // Collations
    'armscii8_bin,armscii8_general_ci,ascii_bin,ascii_general_ci,big5_bin,' +
    'big5_chinese_ci,binary,cp1250_bin,cp1250_croatian_ci,cp1250_czech_cs,' +
    'cp1250_general_ci,cp1250_polish_ci,cp1251_bin,cp1251_bulgarian_ci,' +
    'cp1251_general_ci,cp1251_general_cs,cp1251_ukrainian_ci,cp1256_bin,' +
    'cp1256_general_ci,cp1257_bin,cp1257_general_ci,cp1257_lithuanian_ci,' +
    'cp850_bin,cp850_general_ci,cp852_bin,cp852_general_ci,cp866_bin,' +
    'cp866_general_ci,cp932_bin,cp932_japanese_ci,dec8_bin,dec8_swedish_ci,' +
    'eucjpms_bin,eucjpms_japanese_ci,euckr_bin,euckr_korean_ci,gb18030_bin,' +
    'gb18030_chinese_ci,gb18030_unicode_520_ci,gb2312_bin,gb2312_chinese_ci,' +
    'gbk_bin,gbk_chinese_ci,geostd8_bin,geostd8_general_ci,greek_bin,' +
    'greek_general_ci,hebrew_bin,hebrew_general_ci,hp8_bin,hp8_english_ci,' +
    'keybcs2_bin,keybcs2_general_ci,koi8r_bin,koi8r_general_ci,koi8u_bin,' +
    'koi8u_general_ci,latin1_bin,latin1_danish_ci,latin1_general_ci,' +
    'latin1_general_cs,latin1_german1_ci,latin1_german2_ci,latin1_spanish_ci,' +
    'latin1_swedish_ci,latin2_bin,latin2_croatian_ci,latin2_czech_cs,' +
    'latin2_general_ci,latin2_hungarian_ci,latin5_bin,latin5_turkish_ci,' +
    'latin7_bin,latin7_estonian_cs,latin7_general_ci,latin7_general_cs,' +
    'macce_bin,macce_general_ci,macroman_bin,macroman_general_ci,sjis_bin,' +
    'sjis_japanese_ci,swe7_bin,swe7_swedish_ci,tis620_bin,tis620_thai_ci,' +
    'ucs2_bin,ucs2_croatian_ci,ucs2_czech_ci,ucs2_danish_ci,' +
    'ucs2_esperanto_ci,ucs2_estonian_ci,ucs2_general_ci,' +
    'ucs2_general_mysql500_ci,ucs2_german2_ci,ucs2_hungarian_ci,' +
    'ucs2_icelandic_ci,ucs2_latvian_ci,ucs2_lithuanian_ci,ucs2_persian_ci,' +
    'ucs2_polish_ci,ucs2_romanian_ci,ucs2_roman_ci,ucs2_sinhala_ci,' +
    'ucs2_slovak_ci,ucs2_slovenian_ci,ucs2_spanish2_ci,ucs2_spanish_ci,' +
    'ucs2_swedish_ci,ucs2_turkish_ci,ucs2_unicode_520_ci,ucs2_unicode_ci,' +
    'ucs2_vietnamese_ci,ujis_bin,ujis_japanese_ci,utf16le_bin,' +
    'utf16le_general_ci,utf16_bin,utf16_croatian_ci,utf16_czech_ci,' +
    'utf16_danish_ci,utf16_esperanto_ci,utf16_estonian_ci,utf16_general_ci,' +
    'utf16_german2_ci,utf16_hungarian_ci,utf16_icelandic_ci,utf16_latvian_ci,' +
    'utf16_lithuanian_ci,utf16_persian_ci,utf16_polish_ci,utf16_romanian_ci,' +
    'utf16_roman_ci,utf16_sinhala_ci,utf16_slovak_ci,utf16_slovenian_ci,' +
    'utf16_spanish2_ci,utf16_spanish_ci,utf16_swedish_ci,utf16_turkish_ci,' +
    'utf16_unicode_520_ci,utf16_unicode_ci,utf16_vietnamese_ci,utf32_bin,' +
    'utf32_croatian_ci,utf32_czech_ci,utf32_danish_ci,utf32_esperanto_ci,' +
    'utf32_estonian_ci,utf32_general_ci,utf32_german2_ci,utf32_hungarian_ci,' +
    'utf32_icelandic_ci,utf32_latvian_ci,utf32_lithuanian_ci,' +
    'utf32_persian_ci,utf32_polish_ci,utf32_romanian_ci,utf32_roman_ci,' +
    'utf32_sinhala_ci,utf32_slovak_ci,utf32_slovenian_ci,utf32_spanish2_ci,' +
    'utf32_spanish_ci,utf32_swedish_ci,utf32_turkish_ci,utf32_unicode_520_ci,' +
    'utf32_unicode_ci,utf32_vietnamese_ci,utf8mb4_bin,utf8mb4_croatian_ci,' +
    'utf8mb4_czech_ci,utf8mb4_danish_ci,utf8mb4_esperanto_ci,' +
    'utf8mb4_estonian_ci,utf8mb4_general_ci,utf8mb4_german2_ci,' +
    'utf8mb4_hungarian_ci,utf8mb4_icelandic_ci,utf8mb4_latvian_ci,' +
    'utf8mb4_lithuanian_ci,utf8mb4_persian_ci,utf8mb4_polish_ci,' +
    'utf8mb4_romanian_ci,utf8mb4_roman_ci,utf8mb4_sinhala_ci,' +
    'utf8mb4_slovak_ci,utf8mb4_slovenian_ci,utf8mb4_spanish2_ci,' +
    'utf8mb4_spanish_ci,utf8mb4_swedish_ci,utf8mb4_turkish_ci,' +
    'utf8mb4_unicode_520_ci,utf8mb4_unicode_ci,utf8mb4_vietnamese_ci,' +
    'utf8_bin,utf8_croatian_ci,utf8_czech_ci,utf8_danish_ci,' +
    'utf8_esperanto_ci,utf8_estonian_ci,utf8_general_ci,' +
    'utf8_general_mysql500_ci,utf8_german2_ci,utf8_hungarian_ci,' +
    'utf8_icelandic_ci,utf8_latvian_ci,utf8_lithuanian_ci,utf8_persian_ci,' +
    'utf8_polish_ci,utf8_romanian_ci,utf8_roman_ci,utf8_sinhala_ci,' +
    'utf8_slovak_ci,utf8_slovenian_ci,utf8_spanish2_ci,utf8_spanish_ci,' +
    'utf8_swedish_ci,utf8_turkish_ci,utf8_unicode_520_ci,utf8_unicode_ci,' +
    'utf8_vietnamese_ci';

  // functions
  MySQLFunctions: string =
    'ABS,ACOS,ADDDATE,ADDTIME,AES_DECRYPT,AES_ENCRYPT,ANY_VALUE,AREA,' +
    'ASBINARY,ASCII,ASIN,ASTEXT,ASWKBASWKT,ASYMMETRIC_DECRYPT,' +
    'ASYMMETRIC_DERIVE,ASYMMETRIC_ENCRYPT,ASYMMETRIC_SIGN,ASYMMETRIC_VERIFY,' +
    'ATAN,ATAN2,AVG,BIN,BIT_AND,BIT_COUNT,BIT_LENGTH,BIT_OR,BIT_XOR,BUFFER,' +
    'CAST,CEIL,CEILING,CENTROID,CHAR_LENGTH,CHARACTER_LENGTH,COALESCE,' +
    'COERCIBILITY,COMPRESS,CONCAT,CONCAT_WS,CONNECTION_ID,CONV,CONVERT_TZ,' +
    'CONVEXHULL,COS,COT,COUNT,CRC32,CREATE_ASYMMETRIC_PRIV_KEY,' +
    'CREATE_ASYMMETRIC_PUB_KEY,CREATE_DH_PARAMETERS,CREATE_DIGEST,CROSSES,' +
    'CURDATE,CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,' +
    'CURTIME,DATE_ADD,DATE_FORMAT,DATE_SUB,DATEDIFF,DAY,DAYNAME,DAYOFMONTH,' +
    'DAYOFWEEK,DAYOFYEAR,DECODE,DEGREES,DES_DECRYPT,DES_ENCRYPT,DIMENSION,' +
    'DISJOINT,DISTANCE,ELT,ENCODE,ENCRYPT,ENDPOINT,ENVELOPE,EQUALS,EXP,' +
    'EXPORT_SET,EXTERIORRING,EXTRACT,EXTRACTVALUE,FIELD,FIND_IN_SET,FLOOR,' +
    'FORMAT,FOUND_ROWS,FROM_BASE64,FROM_DAYS,FROM_UNIXTIME,GEOMCOLLFROMTEXT,' +
    'GEOMCOLLFROMWKB,GEOMETRYCOLLECTIONFROMTEXT,GEOMETRYCOLLECTIONFROMWKB,' +
    'GEOMETRYFROMTEXT,GEOMETRYFROMWKB,GEOMETRYN,GEOMETRYTYPE,GEOMFROMTEXT,' +
    'GEOMFROMWKB,GET_FORMAT,GET_LOCK,GLENGTH,GREATEST,GROUP_CONCAT,' +
    'GTID_SUBSET,GTID_SUBTRACT,HEX,HOUR,IFNULL,INET_ATON,INET_NTOA,' +
    'INET6_ATON,INET6_NTOA,INSTR,INTERIORRINGN,INTERSECTS,INTERVAL,' +
    'IS_FREE_LOCK,IS_IPV4,IS_IPV4_COMPAT,IS_IPV4_MAPPED,IS_IPV6,IS_USED_LOCK,' +
    'ISCLOSED,ISEMPTY,ISNULL,ISSIMPLE,JSON_APPEND,JSON_ARRAY,' +
    'JSON_ARRAY_APPEND,JSON_ARRAY_INSERT,JSON_CONTAINS,JSON_CONTAINS_PATH,' +
    'JSON_DEPTH,JSON_EXTRACT,JSON_INSERT,JSON_KEYS,JSON_LENGTH,JSON_MERGE,' +
    'JSON_OBJECT,JSON_QUOTE,JSON_REMOVE,JSON_REPLACE,JSON_SEARCH,JSON_SET,' +
    'JSON_TYPE,JSON_UNQUOTE,JSON_VALID,LAST_DAY,LAST_INSERT_ID,LCASE,LEAST,' +
    'LEFT,LENGTH,LINEFROMTEXT,LINEFROMWKB,LINESTRINGFROMTEXT,' +
    'LINESTRINGFROMWKB,LN,LOAD_FILE,LOCALTI,LOCALTIME,LOCALTIMESTAMP,LOCATE,' +
    'LOG,LOG10,LOG2,LOWER,LPAD,LTRIM,MAKE_SET,MAKEDATE,MAKETIME,' +
    'MASTER_POS_WAIT,MAX,MBRCONTAINS,MBRCOVEREDBY,MBRCOVERS,MBRDISJOINT,' +
    'MBREQUAL,MBREQUALS,MBRINTERSECTS,MBROVERLAPS,MBRTOUCHES,MBRWITHIN,MD5,' +
    'MICROSECOND,MID,MIN,MINUTE,MLINEFROMTEXT,MLINEFROMWKB,MONTH,' +
    'MONTHNAME,MPOINTFROMTEXT,MPOINTFROMWKB,MPOLYFROMTEXT,MPOLYFROMWKB,' +
    'MULTILINESTRINGFROMTEXT,MULTILINESTRINGFROMWKB,MULTIPOINTFROMTEXT,' +
    'MULTIPOINTFROMWKB,MULTIPOLYGONFROMTEXT,MULTIPOLYGONFROMWKB,NAME_CONST,' +
    'NOW,NULLIF,NUMGEOMETRIES,NUMINTERIORRINGS,NUMPOINTS,OCT,OCTET_LENGTH,' +
    'OLD_PASSWORD,ORD,OVERLAPS,PASSWORD,PERIOD_ADD,PERIOD_DIFF,PI,' +
    'POINTFROMTEXT,POINTFROMWKB,POINTN,POLYFROMTEXT,POLYFROMWKB,' +
    'POLYGONFROMTEXT,POLYGONFROMWKB,POSITION,POW,POWER,QUARTER,QUOTE,RADIANS,' +
    'RAND,RANDOM_BYTES,RELEASE_ALL_LOCKS,RELEASE_LOCK,REVERSE,RIGHT,ROUND,' +
    'ROW_COUNT,RPAD,RTRIM,SEC_TO_TIME,SECOND,SESSION_USER,SHA,SHA1,SHA2,SIGN,' +
    'SIN,SLEEP,SOUNDEX,SPACE,SQRT,SRID,ST_AREA,ST_ASBINARY,ST_ASGEOJSON,' +
    'ST_ASTEXT,ST_ASWKB,ST_ASWKT,ST_BUFFER,ST_BUFFER_STRATEGY,ST_CENTROID,' +
    'ST_CONTAINS,ST_CONVEXHULL,ST_CROSSES,ST_DIFFERENCE,ST_DIMENSION,' +
    'ST_DISJOINT,ST_DISTANCE,ST_DISTANCE_SPHERE,ST_ENDPOINT,ST_ENVELOPE,' +
    'ST_EQUALS,ST_EXTERIORRING,ST_GEOHASH,ST_GEOMCOLLFROMTEXT,' +
    'ST_GEOMCOLLFROMTXT,ST_GEOMCOLLFROMWKB,ST_GEOMETRYCOLLECTIONFROMTEXT,' +
    'ST_GEOMETRYCOLLECTIONFROMWKB,ST_GEOMETRYFROMTEXT,ST_GEOMETRYFROMWKB,' +
    'ST_GEOMETRYN,ST_GEOMETRYTYPE,ST_GEOMFROMGEOJSON,ST_GEOMFROMTEXT,' +
    'ST_GEOMFROMWKB,ST_INTERIORRINGN,ST_INTERSECTION,ST_INTERSECTS,' +
    'ST_ISCLOSED,ST_ISEMPTY,ST_ISSIMPLE,ST_ISVALID,ST_LATFROMGEOHASH,' +
    'ST_LENGTH,ST_LINEFROMTEXT,ST_LINEFROMWKB,ST_LINESTRINGFROMTEXT,' +
    'ST_LINESTRINGFROMWKB,ST_LONGFROMGEOHASH,ST_MAKEENVELOPE,' +
    'ST_MLINEFROMTEXT,ST_MLINEFROMWKB,ST_MPOINTFROMTEXT,ST_MPOINTFROMWKB,' +
    'ST_MPOLYFROMTEXT,ST_MPOLYFROMWKB,ST_MULTILINESTRINGFROMTEXT,' +
    'ST_MULTILINESTRINGFROMWKB,ST_MULTIPOINTFROMTEXT,ST_MULTIPOINTFROMWKB,' +
    'ST_MULTIPOLYGONFROMTEXT,ST_MULTIPOLYGONFROMWKB,ST_NUMGEOMETRIES,' +
    'ST_NUMINTERIORRING,ST_NUMINTERIORRINGS,ST_NUMPOINTS,ST_OVERLAPS,' +
    'ST_POINTFROMGEOHASH,ST_POINTFROMTEXT,ST_POINTFROMWKB,ST_POINTN,' +
    'ST_POLYFROMTEXT,ST_POLYFROMWKB,ST_POLYGONFROMTEXT,ST_POLYGONFROMWKB,' +
    'ST_SIMPLIFY,ST_SRID,ST_STARTPOINT,ST_SYMDIFFERENCE,ST_TOUCHES,ST_UNION,' +
    'ST_VALIDATE,ST_WITHIN,ST_X,ST_Y,STARTPOINT,STD,STDDEV,STDDEV_POP,' +
    'STDDEV_SAMP,STR_TO_DATE,STRCMP,SUBDATE,SUBSTR,SUBSTRING,SUBSTRING_INDEX,' +
    'SUBTIME,SUM,SYSDATE,SYSTEM_USER,TAN,TIME_FORMAT,TIME_TO_SEC,TIMEDIFF,' +
    'TIMESTAMPADD,TIMESTAMPDIFF,TO_BASE64,TO_DAYS,TO_SECONDS,TOUCHES,TRIM,' +
    'TRUNCATE,UCASE,UNCOMPRESS,UNCOMPRESSED_LENGTH,UNHEX,UNIX_TIMESTAMP,' +
    'UPDATEXML,UPPER,USER,UTC_DATE,UTC_TIME,UTC_TIMESTAMP,UUID,UUID_SHORT,' +
    'VALIDATE_PASSWORD_STRENGTH,VAR_POP,VAR_SAMP,VARIANCE,VERSION,' +
    'WAIT_FOR_EXECUTED_GTID_SET,WAIT_UNTIL_SQL_THREAD_AFTER_GTIDS,WEEK,' +
    'WEEKDAY,WEEKOFYEAR,WEIGHT_STRING,WITHIN,X,Y,YEARWEEK';

//---Ingres---------------------------------------------------------------------
  // keywords
  IngresKW: string =
    'ABORT,ACTIVATE,ADD,ADDFORM,AFTER,AGGREGATE,ALL,ALTER,AND,APPEND,ARRAY,' +
    'AS,ASC,AT,AUDIT_LOG,AUTHORIZATION,AUTOCOMMIT,AVGU,BEFORE,BEGIN,BETWEEN,' +
    'BREAKDISPLAY,BY,BYREF,CACHE,CALL,CALLFRAME,CALLPROC,CASCADE,CHECK,CLEAR,' +
    'CLEARROW,CLOSE,COLUMN,COMMAND,COMMENT,COMMIT,CONNECT,CONSTRAINT,' +
    'CONTINUE,COPY,COUNTU,CPUFACTOR,CREATE,CURRENT,CURRENT_USER,CURSOR,DATA,' +
    'DATAHANDLER,DATE_FORMAT,DBEVENT,DDL_CONCURRENCY,DEADLOCK,DECLARE,' +
    'DEFAULT,DEFERRED,DEFINE,DELETE,DELETEROW,DESC,DESCRIBE,DESCRIPTOR,' +
    'DESTROY,DIRECT,DISABLE,DISCONNECT,DISPLAY,DISTINCT,DISTRIBUTE,DO,DOWN,' +
    'DROP,ELSE,ELSEIF,ENABLE,END,ENDDATA,ENDDISPLAY,ENDFORMS,ENDIF,ENDLOOP,' +
    'ENDRETRIEVE,ENDSELECT,ENDWHILE,ERROR,ESCAPE,EXCLUDE,EXCLUDING,EXEC,' +
    'EXECUTE,EXISTS,EXIT,FETCH,FIELD,FINALIZE,FOR,FOREIGN,FORMDATA,FORMINIT,' +
    'FORMS,FROM,FULL,GET,GETFORM,GETOPER,GETROW,GLOBAL,GOTO,GRANT,GRANTED,' +
    'HAVING,HELP,HELP_FORMS,HELP_FRS,HELPFILE,IDENTIFIED,IF,IIMESSAGE,' +
    'IIPRINTF,IIPROMPT,IISTATEMENT,IMMEDIATE,IMPORT,IN,INCLUDE,INDEX,' +
    'INDICATOR,INGRES,INITIALIZE,INITTABLE,INNER,INQUIRE_EQUEL,INQUIRE_FORMS,' +
    'INQUIRE_FRS,INQUIRE_INGRES,INSERT,INSERTROW,INSTALLATION,INTEGRITY,INTO,' +
    'IO_TRACE,IS,J_FREESZ1,J_FREESZ2,J_FREESZ3,J_FREESZ4,J_SORTBUFSZ,' +
    'JCPUFACTOR,JOIN,JOINOP,JOURNALING,KEY,LEVEL,LIKE,LINK,LOADTABLE,LOCAL,' +
    'LOCATION,LOCK_TRACE,LOG_TRACE,LOGDBEVENTS,LOGGING,MAXCOST,MAXCPU,' +
    'MAXPAGE,MENUITEM,MESSAGE,MODE,MODIFY,MODULE,MONEY_FORMAT,MONEY_PREC,' +
    'MOVE,NATURAL,NEXT,NODEADLOCK,NOECHO,NOIO_TRACE,NOJIONOP,NOJOURNALING,' +
    'NOLOCK_TRACE,NOLOG_TRACE,NOLOGDBEVENTS,NOLOGGING,NOMAXCOST,NOMAXCPU,' +
    'NOMAXIO,NOMAXPAGE,NOMAXQUERY,NOMAXROW,NOOPTIMIZEONLY,NOPRINTDBEVENTS,' +
    'NOPRINTQRY,NOPRINTRULES,NOQEP,NORULES,NOSQL,NOSTATISTICS,NOT,NOTRACE,' +
    'NULL,OF,ON,ONLY,OPEN,OPTIMIZEONLY,OPTION,OR,ORDER,OUT,PARAM,PERMIT,' +
    'PREPARE,PRESERVE,PRIMARY,PRINT,PRINTDBEVENTS,PRINTQRY,PRINTSCREEN,' +
    'PRIVILEGES,PROCEDURE,PROMPT,PUBLIC,PUT,PUTFORM,PUTOPER,PUTROW,QBUFSIZE,' +
    'QEP,QRY,QUALIFICATION,QUERY_SIZE,RAISE,RANGE,READONLY,REDISPLAY,' +
    'REFERENCES,REFERENCING,REGISTER,RELOCATE,REMOVE,RENAME,REPEAT,REPEATED,' +
    'REPLACE,REPLICATE,RESTRICT,RESULT_STRUCTURE,RESUME,RET_INTO,RETRIEVE,' +
    'RETURN,RETURNING,REVOKE,ROLLBACK,ROWS,RULE,RUN,SAVE,SAVEPOINT,SCHEMA,' +
    'SCREEN,SCROLL,SCROLLDOWN,SCROLLUP,SECTION,SECURITY_ALARM,SECURITY_AUDIT,' +
    'SELECT,SESSION,SET,SET_4GL,SET_EQUAL,SET_FORMS,SET_FRS,SET_INGRES,' +
    'SET_SQL,SHORT_REMARK,SLEEP,SOME,SORT,SORTBUFSIZE,SQL,STATISTICS,STOP,' +
    'SUBMENU,SUMU,SYNONYM,SYSTEM,TABLE,TABLEDATA,TEWMPORARY,THEN,TO,TRACE,' +
    'TRANSACTION,TYPE,UNION,UNIQUE,UNLOADTABLE,UNTIL,UP,UPDATE,USER,USING,' +
    'VALIDATE,VALIDROW,VALUES,VIEW,WHEN,WHENEVER,WHERE,WHILE,WITH,WORK';

  // types
  IngresTypes: string =
    'BYTE,C,CHAR,CHARACTER,DATE,DECIMAL,FLOAT,FLOAT4,FLOAT8,INTEGER,INTEGER1,' +
    'INTEGER2,INTEGER4,LONG,MONEY,OBJECT_KEY,SECURITY_LABEL,SHORT,SMALLINT,' +
    'TABLE_KEY,TEXT,VARCHAR,VARYING';

  // functions
  IngresFunctions: string =
    '_BINTIM,_CPU_MS,_DATE,_DIO_CNT,_ET_SEC,_PFAULT_CNT,_TIME,_VERSION,ABS,' +
    'ANY,ATAN,AUTOCOMMIT_STATE,AVG,BIOCNT,CHAREXTRACT,COLLATION,CONCAT,' +
    'CONNECT_TIME_LIMIT,COS,COUNT,CREATE_PROCEDURE,CREATE_TABLE,DATABASE,' +
    'DATE_GMT,DATE_PART,DATE_TRUNC,DB_ADMIN,DB_DELIMITED_CASE,DB_NAME_CASE,' +
    'DBA,DBMS_BIO,DBMS_CPU,DBMS_DIO,DBMSINFO,DOW,EXP,FLATTEN_AGGREGATE,' +
    'FLATTEN_NONE,FLATTEN_OPTIMIZE,FLATTEN_SINGLETON,GROUP,HEX,' +
    'IDLE_TIME_LIMIT,IFNULL,INITIAL_USER,INQUIRE_SQL,INT1,INT2,INT4,INTERVAL,' +
    'LANGUAGE,LEFT,LENGTH,LOCATE,LOCKMODE,LOG,LONG_BYTE,LONG_VARCHAR,' +
    'LOWERCASE,MAX,MAXCONNECT,MAXIDLE,MAXIO,MAXQUERY,MAXROW,MIN,MOD,NOTRIM,' +
    'ON_ERROR_STATE,PAD,QUERY_IO_LIMIT,QUERY_LANGUAGE,QUERY_ROW_LIMIT,RIGHT,' +
    'ROLE,SECURITY_AUDIT_LOG,SECURITY_AUDIT_STATE,SECURITY_PRIV,' +
    'SELECT_SYSCAT,SERVER_CLASS,SESSION_ID,SESSION_PRIORITY,' +
    'SESSION_PRIORITY_LIMIT,SESSION_PRIV,SESSION_SECLABEL,SESSION_USER,SHIFT,' +
    'SIN,SIZE,SQRT,SQUEEZE,SUM,SYSTEM_USER,TABLE_STATISTICS,TERMINAL,' +
    'TRANSACTION_STATE,TRIM,UPDATE_ROWCNT,UPDATE_SYSCAT,UPPERCASE,USERNAME,' +
    'VARBYTE';

//---Nexus----------------------------------------------------------------------
  // keywords
  NexusKW: string =
    'ABSOLUTE,AFTER,ALTER,ANY,ASC,ASSERT,ATOMIC,' +
    'ADD,ALL,AND,AS,ASSEMBLY,AUTHORIZATION,BEFORE,' +
    'BETWEEN,BINARY,BLOCK,BY,BEGIN,' +
    'BLOCKSIZE,CALL,CASCADE,CAST,,' +
    'CHARACTERS,CLR,CLOSE,CODEPAGE,COLLATION,COMMIT,CONTAINS,' +
    'CROSS,CALLED,CASE,CATCH,' +
    'CHECK,COALESCE,COLLATE,COLUMN,CONSTRAINT,' +
    'CREATE,CURSOR,DATA,DECLARE,' +
    'DELETE,DESC,DETERMINISTIC,DO,DROP,DAY,DEFAULT,DELETING,' +
    'DESCRIPTION,DISTINCT,EACH,ELSEIF,ENCRYPT,END,EQUIVALENT,' +
    'ESCAPE,EXECUTE,EXISTS,ELSE,EMPTY,ENCRYPTION,ENGINE,' +
    'EXCEPT,EXTERNAL,FALSE,FETCH,FETCH_STATUS,FOR,FROM,FUNCTION,FIRST,FOREIGN,' +
    'FULL,HAVING,HOUR,GLOBAL,GROW,GROUP,GROWSIZE,IDENTITY,IGNORE,' +
    'IMMEDIATE,IN,INITIAL,INNER,INPUT,INSERTING,INTERVAL,IS,IF,INDEX,' +
    'INITIALSIZE,INOUT,INSERT,INTERSECT,INTO,ITERATE,JOIN,' +
    'KANA,KEY,LANGUAGE,LEAVE,LIKE,LOCALE,' +
    'LARGE,LAST,LEFT,LOCAL,MATCH,' +
    'MINUTE,MODIFIES,MONTH,NAME,NATURAL,NEXT,NONSPACE,' +
    'NULLIF,NATIONAL,' +
    'NEW,NO,NORESTRICT,NOT,NULL,NULLS,OBJECT,OCTETS,OF,ON,OUT,OCTET_LENGTH,' +
    'ODD,OLD,OPEN,OR,ORDER,OUTER,PARTIAL,PERCENT,PRECISION,' +
    'PRIOR,PROCEDURE,PASSWORDS,PRIMARY,REFERENCES,RELATIVE,REMOVE,RESTRICT,' +
    'RETURNS,ROLLBACK,ROUTINE,READS,' +
    'REFERENCING,REPEAT,RETURN,RIGHT,ROW,SECOND,' +
    'SERIALIZABLE,SET,SIMPLE,SNAPSHOT,SORT,' +
    'STRING,SELECT,SIGNAL,' +
    'SOME,SQL,START,STORAGE,SYMBOLS,TABLE,' +
    'TOP,TRANSACTION,TRY,THEN,' +
    'TO,TRIGGER,TRUE,TYPE,UNION,UNKNOWN,UPDATE,' +
    'UNIQUE,UNTIL,UPDATING,USE,VALUES,VARYING,' +
    'VIEW,WHEN,WHILE,WITH,WORK,WHERE,WIDTH,YEAR';

  // functions
  NexusFunctions: string =
    'ABS,ATAN,ATAN2,ATN2,AVG,BOTH,BROUND,CEIL,CEILING,CHAR_LENGTH,CHARACTER_LENGTH,'+
    'CHR,COS,COUNT,CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,ERROR_MESSAGE,EXP,EXTRACT,'+
    'FLOOR,LASTAUTOINC,LEADING,LIST,LN,LOCALTIME,LOCALTIMESTAMP,LOWER,MAX,MED,MIN,MOD,NEWGUID,OCTECT,'+
    'OCTECT_LENGTH,ORD,PI,POSITION,POWER,RAND,ROUND,ROWSAFFECTED,ROWSREAD,SESSION_USER,SIN,SQRT,STD,'+
    'SUBSTRING,SUM,SYSTEM_ROW#,TOSTRING,TOSTRINGLEN,TRAILING,TRIM,UPPER,USER,USING';

  // types
  NexusTypes: string =
    'CHARACTER,CHAR,NULLSTRING,SHORTSTRING,SINGLECHAR,VARCHAR,' +
    'CLOB,TEXT,NSINGLECHAR,NCHAR,' +
    'NVARCHAR,NCLOB,BLOB,IMAGE,NUMERIC,DECIMAL,DEC,BYTE,TINYINT,SHORTINT,SMALLINT,INTEGER,INT,' +
    'AUTOINC,BIGINT,LARGEINT,WORD,DWORD,FLOAT,REAL,DOUBLE,EXTENDED,MONEY,' +
    'BOOLEAN,BOOL,DATE,TIME,TIMESTAMP,DATETIME,GUID,BYTEARRAY,RECREV';

//---Informix-------------------------------------------------------------------
  // keywords
  InformixKW: string =
    'ABSOLUTE,ACCESS,ACCESS_METHOD,ACTIVE,ADD,AFTER,AGGREGATE,ALIGNMENT,ALL,' +
    'ALL_ROWS,ALLOCATE,ALTER,AND,ANSI,ANY,APPEND,AS,ASC,AT,ATTACH,AUDIT,' +
    'AUTHORIZATION,AUTO,AUTOFREE,AVOID_EXECUTE,AVOID_SUBQF,BEFORE,BEGIN,' +
    'BETWEEN,BINARY,BOTH,BUFFERED,BUILTIN,BY,CACHE,CANNOTHASH,CASCADE,CHECK,' +
    'CLASS,CLIENT,CLOSE,CLUSTER,CLUSTERSIZE,COARSE,COBOL,CODESET,COLLATION,' +
    'COLLECTION,COLUMN,COMMIT,COMMITTED,COMMUTATOR,CONCURRENT,CONNECT,' +
    'CONNECTION,CONST,CONSTRAINT,CONSTRAINTS,CONSTRUCTOR,COPY,COSTFUNC,' +
    'CRCOLS,CREATE,CROSS,CURRENT_ROLE,CURSOR,CYCLE,DATABASE,DATAFILES,' +
    'DATASKIP,DBA,DBDATE,DBPASSWORD,DEALLOCATE,DEBUG,DEC_T,DECLARE,DEFAULT,' +
    'DEFERRED,DEFERRED_PREPARE,DELAY,DELETE,DELIMITER,DELUXE,DEREF,DESC,' +
    'DESCRIBE,DESCRIPTOR,DETACH,DIAGNOSTICS,DIRECTIVES,DIRTY,DISABLED,' +
    'DISCONNECT,DISTINCT,DISTRIBUTEBINARY,DISTRIBUTESREFERENCES,' +
    'DISTRIBUTIONS,DOCUMENT,DOMAIN,DONOTDISTRIBUTE,DORMANT,DROP,DTIME_T,EACH,' +
    'ELIF,ELSE,ENABLED,ENCRYPTION,END,ENUM,ENVIRONMENT,ESCAPE,EXCLUSIVE,EXEC,' +
    'EXECUTE,EXECUTEANYWHERE,EXISTS,EXPLAIN,EXPLICIT,EXPRESS,EXPRESSION,' +
    'EXTENT,EXTERNAL,FALSE,FAR,FETCH,FILE,FILLFACTOR,FILTERING,FIRST,' +
    'FIRST_ROWS,FIXCHAR,FIXED,FLUSH,FOREIGN,FORMAT,FORTRAN,FOUND,FRACTION,' +
    'FRAGMENT,FREE,FROM,FULL,FUNCTION,GENERAL,GET,GK,GLOBAL,GO,GOTO,GRANT,' +
    'GROUP,HANDLESNULLS,HASH,HAVING,HIGH,HINT,HOLD,HOUR,HYBRID,IFX_INT8_T,' +
    'IFX_LO_CREATE_SPEC_T,IFX_LO_STAT_T,IMMEDIATE,IMPLICIT,IN,INACTIVE,' +
    'INCREMENT,INDEX,INDEXES,INDICATOR,INFORMIX,INIT,INLINE,INNER,INOUT,' +
    'INSERT,INSTEAD,INTEG,INTERNAL,INTERNALLENGTH,INTO,INTRVL_T,IS,' +
    'ISCANONICAL,ISOLATION,ITEM,ITERATOR,JOIN,KEEP,KEY,LABELEQ,LABELGE,' +
    'LABELGLB,LABELGT,LABELLE,LABELLT,LABELLUB,LABELTOSTRING,LANGUAGE,LAST,' +
    'LEADING,LEFT,LEVEL,LIKE,LIMIT,LISTING,LOAD,LOC_T,LOCAL,LOCATOR,LOCK,' +
    'LOCKS,LOG,LONG,LOW,MATCHES,MAXERRORS,MAXLEN,MAXVALUE,MEDIAN,MEDIUM,' +
    'MEMORY_RESIDENT,MIDDLE,MINUTE,MINVALUE,MODE,MODERATE,MODIFY,MODULE,' +
    'MOUNTING,NAME,NEGATOR,NEW,NEXT,NO,NOCACHE,NOCYCLE,NOMAXVALUE,NOMIGRATE,' +
    'NOMINVALUE,NON_RESIDENT,NONE,NOORDER,NORMAL,NOT,NOTEMPLATEARG,NULL,' +
    'OF,OFF,OLD,ONLINE,ONLY,OPAQUE,OPCLASS,OPEN,OPERATIONAL,OPTCOMPIND,' +
    'OPTICAL,OPTIMIZATION,OPTION,OR,ORDER,OUT,OUTER,OUTPUT,PAGE,' +
    'PARALLELIZABLE,PARAMETER,PARTITION,PASCAL,PASSEDBYVALUE,PASSWORD,' +
    'PDQPRIORITY,PERCALL_COST,PLI,PLOAD,PREPARE,PREVIOUS,PRIMARY,PRIOR,' +
    'PRIVATE,PRIVILEGES,PROCEDURE,PUBLIC,PUT,RAW,READ,RECORDEND,REF,' +
    'REFERENCES,REFERENCING,REGISTER,REJECTFILE,RELATIVE,RELEASE,REMAINDER,' +
    'RENAME,REOPTIMIZATION,REPEATABLE,REPLICATION,RESERVE,RESOLUTION,' +
    'RESOURCE,RESTART,RESTRICT,RESUME,RETAIN,RETURNING,RETURNS,REUSE,REVOKE,' +
    'RIGHT,ROBIN,ROLE,ROLLBACK,ROLLFORWARD,ROUTINE,ROWID,ROWIDS,ROWS,SAMEAS,' +
    'SAMPLES,SAVE,SCHEDULE,SCHEMA,SCRATCH,SCROLL,SECOND,SECONDARY,SECTION,' +
    'SELCONST,SELECT,SELFUNC,SEQUENCE,SERIALIZABLE,SERVERUUID,SESSION,SHARE,' +
    'SHORT,SIGNED,SIZE,SKALL,SKINHIBIT,SKIP,SKSHOW,SOME,SPECIFIC,SQL,SQLCODE,' +
    'SQLCONTEXT,SQLERROR,SQLSTATE,SQLWARNING,STABILITY,STACK,STANDARD,START,' +
    'STATIC,STATISTICS,STEP,STOP,STORAGE,STRATEGIES,STRING,STRINGTOLABEL,' +
    'STRUCT,STYLE,SUPPORT,SYNC,SYNONYM,TABLE,TEMP,TEMPLATE,TEST,TIME,TIMEOUT,' +
    'TO,TRAILING,TRANSACTION,TRIGGER,TRIGGERS,TRUE,TRUNCATE,TYPE,TYPEDEF,' +
    'TYPEID,TYPENAME,TYPEOF,UNCOMMITTED,UNDER,UNION,UNIQUE,UNKNOWN,UNLOAD,' +
    'UNLOCK,UNSIGNED,UPDATE,USAGE,USE_SUBQF,USING,VALUE,VALUES,VAR,VARIABLE,' +
    'VARIANT,VIEW,VIOLATIONS,VOID,VOLATILE,WAIT,WARNING,WHERE,WITH,WITHOUT,' +
    'WORK,WRITE,XADATASOURCE,XID,XLOAD,XUNLOAD';

  // types
  InformixTypes: string =
    'BLOB,BOOLEAN,BYTE,CHAR,CHARACTER,VARYING,CLOB,DATE,DATETIME,DEC,DECIMAL,' +
    'DOUBLE,PRECISION,FLOAT,INT,INT8,INTEGER,INTERVAL,LIST,LVARCHAR,MONEY,' +
    'MULTISET,NCHAR,NUMERIC,NVARCHAR,REAL,ROW,SERIAL,SERIAL8,SET,SMALLFLOAT,' +
    'SMALLINT,TEXT,VARCHAR';

  // PLSQL keywords
  InformixPLSQLKW: string =
    'CALL,CONTINUE,DEFINE,ERROR,EXCEPTION,EXIT,FOR,FOREACH,IF,LET,ON,RAISE,' +
    'RETURN,SYSTEM,TRACE,WHENEVER,WHILE';

  // functions
  InformixFunctions: string =
    'ABS,ACOS,ASIN,ATAN,ATAN2,AVG,CARDINALITY,CASE,CAST,CHARACTER_LENGTH,' +
    'CHAR_LENGTH,COS,COUNT,CURRENT,DAY,DBINFO,DBSERVERNAME,DECODE,' +
    'DECRYPT_CHAR,DECRYPT_BINARY,DEFAULT_ROLE,ENCRYPT_AES,ENCRYPT_TDES,EXP,' +
    'EXTEND,FILETOBLOB,FILETOCLOB,GETHINT,HEX,IFX_ALLOW_NEWLINE,' +
    'IFX_REPLACE_MODULE,INITCAP,LENGTH,LOCOPY,LOGN,LOG10,LOTOFILE,LOWER,LPAD,' +
    'MAX,MDY,MIN,MOD,MONTH,NVL,OCTET_LENGTH,POW,RANGE,REPLACE,ROOT,ROUND,' +
    'RPAD,SIN,SITENAME,SQRT,STDEV,SUBSTR,SUBSTRING,SUM,TAN,THEN,TO_CHAR,TO_DATE,' +
    'TODAY,TRIM,TRUNC,UNITS,UPPER,USER,VARIANCE,WEEKDAY,WHEN,YEAR';

function TSynSQLSyn.HashKey(Str: PWideChar): Integer;
var
  FoundDoubleMinus: Boolean;

  function GetOrd: Integer;
  begin
    case Str^ of
      '_': Result := 1;
      'a'..'z': Result := 2 + Ord(Str^) - Ord('a');
      'A'..'Z': Result := 2 + Ord(Str^) - Ord('A');
      '@':
        if fDialect in [sqlMSSQL7, sqlMSSQL2K] then
          Result := 24
        else
          Result := 0;
      else Result := 0;
    end;
  end;

begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    FoundDoubleMinus := (Str^ = '-') and ((Str + 1)^ = '-');
    if FoundDoubleMinus then Break;
{$IFOPT Q-}
    Result := 2 * Result + GetOrd;
{$ELSE}
    Result := (2 * Result + GetOrd) and $FFFFFF;
{$ENDIF}
    Inc(Str);
  end;
  Result := Result and $FF; // 255
  if Assigned(fToIdent) then
    fStringLen := Str - fToIdent
  else
    fStringLen := 0;
end;

function TSynSQLSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > fStringLen then
      Break
    else if Entry.KeywordLen = fStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        Exit;
      end;
    Entry := Entry.Next;
  end;
  if fTableDict.ContainsKey(SysUtils.AnsiLowerCase(Copy(StrPas(fToIdent), 1, fStringLen))) then
    Result := tkTableName
  else
    Result := tkIdentifier;
end;

constructor TSynSQLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fKeywords := TSynHashEntryList.Create;

  FProcNames := TStringList.Create;
  TStringList(FProcNames).OnChange := ProcNamesChanged;

  fTableNames := TStringList.Create;
  TStringList(fTableNames).OnChange := TableNamesChanged;
  FTableDict := TDictionary<string, Boolean>.Create;

  fFunctionNames := TStringList.Create;
  TStringList(fFunctionNames).OnChange := FunctionNamesChanged;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fConditionalCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrConditionalComment, SYNS_FriendlyAttrConditionalComment);
  fConditionalCommentAttri.Style := [fsItalic];
  AddAttribute(fConditionalCommentAttri);
  fConsoleOutputAttri := TSynHighlighterAttributes.Create(SYNS_AttrConsoleOutput, SYNS_FriendlyAttrConsoleOutput);
  fConsoleOutputAttri.Style := [fsBold, fsUnderline];
  AddAttribute(fConsoleOutputAttri);
  fDataTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  fDataTypeAttri.Style := [fsBold];
  AddAttribute(fDataTypeAttri);
  fDefaultPackageAttri :=
    TSynHighlighterAttributes.Create(SYNS_AttrDefaultPackage, SYNS_FriendlyAttrDefaultPackage);
  fDefaultPackageAttri.Style := [fsBold];
  AddAttribute(fDefaultPackageAttri);
  fDelimitedIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrDelimitedIdentifier, SYNS_FriendlyAttrDelimitedIdentifier);
  AddAttribute(fDelimitedIdentifierAttri);
  fExceptionAttri := TSynHighlighterAttributes.Create(SYNS_AttrException, SYNS_FriendlyAttrException);
  fExceptionAttri.Style := [fsItalic];
  AddAttribute(fExceptionAttri);
  fFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  fFunctionAttri.Style := [fsBold];
  AddAttribute(fFunctionAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fPLSQLAttri := TSynHighlighterAttributes.Create(SYNS_AttrPLSQL, SYNS_FriendlyAttrPLSQL);
  fPLSQLAttri.Style := [fsBold];
  AddAttribute(fPLSQLAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fSQLPlusAttri:=TSynHighlighterAttributes.Create(SYNS_AttrSQLPlus, SYNS_FriendlyAttrSQLPlus);
  fSQLPlusAttri.Style := [fsBold];
  AddAttribute(fSQLPlusAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_Attrstring, SYNS_FriendlyAttrstring);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fProcNameAttri := TSynHighlighterAttributes.Create(SYNS_AttrProcName, SYNS_FriendlyAttrProcName);
  AddAttribute(fProcNameAttri);
  fTableNameAttri := TSynHighlighterAttributes.Create(SYNS_AttrTableName, SYNS_FriendlyAttrTableName);
  AddAttribute(fTableNameAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterSQL;
  fRange := rsUnknown;
  fDialect := sqlStandard;
  InitializeKeywordLists;
end;

destructor TSynSQLSyn.Destroy;
begin
  fKeywords.Free;
  fProcNames.Free;
  fTableNames.Free;
  fTableDict.Free;
  fFunctionNames.Free;
  inherited Destroy;
end;

procedure TSynSQLSyn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if (Source is TSynSQLSyn) then
    SQLDialect := TSynSQLSyn(Source).SQLDialect;
end;

procedure TSynSQLSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '&']) then Inc(Run);
end;

procedure TSynSQLSyn.AsciiCharProc;
var
  IsEsc: Boolean;
begin
  // Oracle SQL allows strings to go over multiple lines
  if fLine[Run] = #0 then
    NullProc
  else begin
    fTokenID := tkString;
    // else it's end of multiline string
    if SQLDialect <> sqlMySql then
    begin
      if (Run > 0) or (fRange <> rsString) or (fLine[Run] <> #39) then
      begin
        fRange := rsString;
        repeat
          Inc(Run);
        until IsLineEnd(Run) or (fLine[Run] = #39);
      end;
      if fLine[Run] = #39 then
      begin
        Inc(Run);
        fRange := rsUnknown;
      end;
    end
    else
    begin
      IsEsc := False;
      if (Run > 0) or (fRange <> rsString) or
        ((fLine[Run] <> #39) and (fLine[Run - 1] <> '\')) then
      begin
        fRange := rsString;
        repeat
          if FLine[Run] = '\' then
            IsEsc := not IsEsc
          else
            IsEsc := False;
          if (not IsEsc) and (FLine[Run + 1] = #39) then
          begin
            Inc(Run);
            Break;
          end;
          Inc(Run);
        until IsLineEnd(Run);
      end;
      if (FLine[Run] = #39) and (not IsEsc) then
      begin
        Inc(Run);
        fRange := rsUnknown;
      end;
    end;
  end;
end;

procedure TSynSQLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynSQLSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '>']) then Inc(Run);
end;

procedure TSynSQLSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '>']) then Inc(Run);
end;

procedure TSynSQLSyn.IdentProc;
var
  FoundDoubleMinus: Boolean;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  if FTokenID in [tkComment, tkConsoleOutput] then
  begin
    while not IsLineEnd(Run) do
      Inc(Run);
  end
  else
    while IsIdentChar(fLine[Run]) do
    begin
      FoundDoubleMinus := (fLine[Run] = '-') and (fLine[Run + 1] = '-');
      if FoundDoubleMinus then Break;
      Inc(Run);
    end;
end;

procedure TSynSQLSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynSQLSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  case fLine[Run] of
    '=': Inc(Run);
    '<': begin
           Inc(Run);
           if fLine[Run] = '=' then Inc(Run);
         end;
  end;
end;

procedure TSynSQLSyn.MinusProc;
begin
  Inc(Run);
  if (fLine[Run] = '-') and ((fDialect <> sqlMySQL) or (fLine[Run+1] = ' ')) then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynSQLSyn.HashProc;
begin
  if SQLDialect = sqlMySql then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  begin
    Inc(Run);
    fTokenID := tkUnknown;
  end;
end;

procedure TSynSQLSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynSQLSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', '-':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then
          Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynSQLSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '|']) then Inc(Run);
end;

procedure TSynSQLSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '+']) then Inc(Run);
end;

procedure TSynSQLSyn.FunctionNamesChanged(Sender: TObject);
begin
  InitializeKeywordLists;
end;

procedure TSynSQLSyn.ProcNamesChanged(Sender: TObject);
begin
  InitializeKeywordLists;
end;

procedure TSynSQLSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        if (SQLDialect = sqlMySql) and (fLine[Run + 1] = '!') then
        begin
          fRange := rsConditionalComment;
          fTokenID := tkConditionalComment;
        end
        else
        begin
          fRange := rsComment;
          fTokenID := tkComment;
        end;
        repeat
          Inc(Run);
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
            fRange := rsUnknown;
            Inc(Run, 2);
            Break;
          end;
        until IsLineEnd(Run);
      end;
    '=':
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
    '/':
      begin
        if (SQLDialect = sqlNexus)  then
        begin
          fTokenID := tkComment;
          repeat
            Inc(Run);
          until IsLineEnd(Run);
        end;
      end
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynSQLSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynSQLSyn.QuoteProc;
begin
  fTokenID := tkDelimitedIdentifier;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    if fLine[Run] = #34 then
    begin
      Inc(Run);
      if fLine[Run] <> #34 then
        Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynSQLSyn.BacktickProc;
begin
  if SQLDialect = sqlMySql then
  begin
    fTokenID := tkDelimitedIdentifier;
    Inc(Run);
    while not IsLineEnd(Run) do
    begin
      if fLine[Run] = '`' then
      begin
        Inc(Run);
        if fLine[Run] <> '`' then
          Break;
      end;
      Inc(Run);
    end;
  end
  else
  begin
    Inc(Run);
    fTokenID := tkUnknown;
  end;
end;

procedure TSynSQLSyn.BracketProc;
begin
  if SQLDialect in [sqlMSSQL7, sqlMSSQL2K] then
  begin
    fTokenID := tkDelimitedIdentifier;
    Inc(Run);
    while not IsLineEnd(Run) do
    begin
      if fLine[Run] = ']' then
      begin
        Inc(Run);
        if fLine[Run] <> ']' then
          Break;
      end;
      Inc(Run);
    end;
  end
  else
  begin
    Inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynSQLSyn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSQLSyn.SymbolAssignProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSynSQLSyn.VariableProc;
var
  Int: Integer;
  FoundDoubleMinus: Boolean;
begin
  // MS SQL Server uses @@ to indicate system functions/variables
  if (SQLDialect in [sqlMSSQL7, sqlMSSQL2K]) and (fLine[Run] = '@') and (fLine[Run + 1] = '@') then
    IdentProc
  else if (SQLDialect in [sqlMySql, sqlOracle]) and (fLine[Run] = '@') then
    SymbolProc
  // Oracle uses the ':' character to indicate bind variables
  // Ingres II also uses the ':' character to indicate variables
  else if not (SQLDialect in [sqlOracle, sqlIngres]) and (fLine[Run] = ':') then
    SymbolProc
  else
  begin
    fTokenID := tkVariable;
    Int := Run;
    repeat
      FoundDoubleMinus := (fLine[Int] = '-') and (fLine[Int + 1] = '-');
      if FoundDoubleMinus then Break;
      Inc(Int);
    until not IsIdentChar(fLine[Int]);
    Run := Int;
  end;
end;

procedure TSynSQLSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSQLSyn.AnsiCProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else
    begin
      if fRange = rsConditionalComment then
        fTokenID := tkConditionalComment
      else
        fTokenID := tkComment;
      repeat
        if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
        begin
          fRange := rsUnknown;
          Inc(Run, 2);
          Break;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

function TSynSQLSyn.IsKeyword(const AKeyword: string): Boolean;
var
  tk: TtkTokenKind;
begin
  tk := IdentKind(PWideChar(AKeyword));
  Result := tk in [tkDatatype, tkException, tkFunction, tkKey, tkPLSQL,
    tkDefaultPackage];
end;

procedure TSynSQLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment, rsConditionalComment:
      AnsiCProc;
    rsConsoleOutput:
      begin
        while not IsLineEnd(Run) do
          Inc(Run);
      end;
    rsString:
      AsciiCharProc;
  else
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #39: AsciiCharProc;
      '=': EqualProc;
      '>': GreaterProc;
      '<': LowerProc;
      '-': MinusProc;
      '#': HashProc;
      '|': OrSymbolProc;
      '+': PlusProc;
      '/': SlashProc;
      '&': AndSymbolProc;
      #34: QuoteProc;
      '`': BacktickProc;
      '[': BracketProc;
      ':', '@': VariableProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '0'..'9': NumberProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '^', '%', '*', '!': SymbolAssignProc;
      '{', '}', '.', ',', ';', '?', '(', ')', ']', '~': SymbolProc;
      else UnknownProc;
    end;
  end;
  inherited;
end;

function TSynSQLSyn.GetDefaultAttribute(Index: Integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSQLSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynSQLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynSQLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynSQLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkConditionalComment: Result := fConditionalCommentAttri;
    tkConsoleOutput: Result := FConsoleOutputAttri;
    tkDatatype: Result := fDataTypeAttri;
    tkDefaultPackage: Result := fDefaultPackageAttri;
    tkDelimitedIdentifier: Result := fDelimitedIdentifierAttri;
    tkException: Result := fExceptionAttri;
    tkFunction: Result := fFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkPLSQL: Result := fPLSQLAttri;
    tkSpace: Result := fSpaceAttri;
    tkSQLPlus: Result := fSQLPlusAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkProcName: Result := FProcNameAttri;
    tkTableName: Result := fTableNameAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynSQLSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynSQLSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynSQLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynSQLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterSQL;
end;

function TSynSQLSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    'a'..'z', 'A'..'Z', '0'..'9', '_':
      Result := True;
    '-':
      Result := fDialect = sqlStandard;
    '#', '$':                          // TODO: check this case, ANSI code wasn't clear here if this is exclusively Oracle
      Result := fDialect in [sqlOracle, sqlNexus];
    '@':
      Result := fDialect in [sqlMSSQL7, sqlMSSQL2K];
     '!', '^', '{', '}','~':
      Result := fDialect = sqlNexus
    else
      Result := False;
  end;
end;

class function TSynSQLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

procedure TSynSQLSyn.DoAddKeyword(AKeyword: string; AKind: Integer);
var
  HashValue: Integer;
begin
  AKeyword := SysUtils.AnsiLowerCase(AKeyword);
  HashValue := HashKey(PWideChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

procedure TSynSQLSyn.SetTableNames(const Value: TStrings);
begin
  fTableNames.Assign(Value);
end;

procedure TSynSQLSyn.TableNamesChanged(Sender: TObject);
begin
  InitializeKeywordLists;
end;

procedure TSynSQLSyn.PutTableNamesInKeywordList;
var
  Int: Integer;
  Entry: TSynHashEntry;
begin
  for Int := 0 to fTableNames.Count - 1 do
  begin
    Entry := fKeywords[HashKey(PWideChar(fTableNames[Int]))];
    while Assigned(Entry) do
    begin
      if SysUtils.AnsiLowerCase(Entry.Keyword) = SysUtils.AnsiLowerCase(fTableNames[Int]) then
        Break;
      Entry := Entry.Next;
    end;
    if not Assigned(Entry) then
      if not fTableDict.ContainsKey(SysUtils.AnsiLowerCase(fTableNames[Int])) then
        FTableDict.Add(SysUtils.AnsiLowerCase(FTableNames[Int]), True);
  end;
end;

procedure TSynSQLSyn.PutFunctionNamesInKeywordList;
var
  Int: Integer;
  Entry: TSynHashEntry;
begin
  for Int := 0 to (fFunctionNames.Count - 1) do
  begin
    Entry := fKeywords[HashKey(PWideChar(fFunctionNames[Int]))];
    while Assigned(Entry) do
    begin
      if SysUtils.AnsiLowerCase(Entry.Keyword) = SysUtils.AnsiLowerCase(fFunctionNames[Int]) then
        Break;
      Entry := Entry.Next;
    end;
    if not Assigned(Entry) then
      DoAddKeyword(fFunctionNames[Int], Ord(tkFunction));
  end;
end;

procedure TSynSQLSyn.PutProcNamesInKeywordList;
var
  Int: Integer;
  Entry: TSynHashEntry;
begin
  for Int := 0 to fProcNames.Count - 1 do
  begin
    Entry := fKeywords[HashKey(PWideChar(FProcNames[Int]))];
    while Assigned(Entry) do
    begin
      if SysUtils.AnsiLowerCase(Entry.Keyword) = SysUtils.AnsiLowerCase(FProcNames[Int]) then
        Break;
      Entry := Entry.Next;
    end;
    if not Assigned(Entry) then
      DoAddKeyword(fProcNames[Int], Ord(tkProcName));
  end;
end;

procedure TSynSQLSyn.InitializeKeywordLists;
var
  Int: Integer;
begin
  fKeywords.Clear;
  fTableDict.Clear;
  fToIdent := nil;

  for Int := 0 to Ord(High(TtkTokenKind)) do
    EnumerateKeywords(Int, GetKeywords(Int), IsIdentChar, DoAddKeyword);

  PutProcNamesInKeywordList;
  PutTableNamesInKeywordList;
  PutFunctionNamesInKeywordList;
  DefHighlightChange(Self);
end;

procedure TSynSQLSyn.SetDialect(Value: TSQLDialect);
begin
  if (Value <> fDialect) then
  begin
    fDialect := Value;
    InitializeKeywordLists;
  end;
end;

procedure TSynSQLSyn.SetFunctionNames(const Value: TStrings);
begin
  FFunctionNames.Assign(Value);
end;

procedure TSynSQLSyn.SetProcNames(const Value: TStrings);
begin
  fProcNames.Assign(Value);
end;

function TSynSQLSyn.GetSampleSource: string;
begin
  Result := '';
  case fDialect of
    sqlPostgres:
      Result := '-- ANSI SQL sample source'#13#10 +
        'SELECT *'#13#10 +
        'FROM planets'#13#10 +
        'WHERE diameter < 13000'#13#10 +
        '  AND name <> ''Earth''';
    sqlStandard:
      Result := '-- ANSI SQL sample source'#13#10 +
        'SELECT *'#13#10 +
        'FROM planets'#13#10 +
        'WHERE diameter < 13000'#13#10 +
        '  AND name <> ''Earth''';
    sqlInterbase6:
      Result := '/* Interbase sample source */'#13#10 +
        'SET TERM !! ;'#13#10 +
        #13#10 +
        'CREATE PROCEDURE HelloWorld(P_MSG VARCHAR(80)) AS'#13#10 +
        'BEGIN'#13#10 +
        '  EXECUTE PROCEDURE WRITELN(:P_MSG);'#13#10 +
        'END !!'#13#10 +
        #13#10 +
        'SET TERM ; !!';
    sqlMySQL:
      Result := '/* MySQL sample source*/'#13#10 +
        'SET @variable = 1;'#13#10 +
        #13#10 +
        'CREATE /*!32302 TEMPORARY */ TABLE t (a INT);'#13#10 +
        #13#10 +
        'CREATE TABLE sample ('#13#10 +
        '        id INT NOT NULL,'#13#10 +
        '        first_name CHAR(30) NOT NULL,'#13#10 +
        '        PRIMARY KEY (id),'#13#10 +
        '        INDEX name (first_name));'#13#10 +
        #13#10 +
        'SELECT DATE_ADD(''1997-12-31 23:59:59'','#13#10 +
        '        INTERVAL 1 SECOND);'#13#10 +
        #13#10 +
        '# End of sample';
    sqlOracle:
      Result := 'PROMPT Oracle sample source'#13#10 +
        'declare'#13#10 +
        '  x varchar2(2000);'#13#10 +
        'begin   -- Show some text here'#13#10 +
        '  select to_char(count(*)) into x'#13#10 +
        '  from tab;'#13#10 +
        #13#10 +
        '  dbms_output.put_line(''Hello World: '' || x);'#13#10 +
        'exception'#13#10 +
        '  when others then'#13#10 +
        '    null;'#13#10 +
        'end;';
    sqlSybase:
      Result := '/* SyBase example source */'#13#10 +
        'declare @Integer        int'#13#10 +
        #13#10 +
        '/* Good for positive numbers only. */'#13#10 +
        'select @Integer = 1000'#13#10 +
        #13#10 +
        'select "Positives Only" ='#13#10 +
        '  right(replicate("0",12) + '#13#10 +
        '    convert(varchar, @Integer),12)'#13#10 +
        #13#10 +
        '/* Good for positive and negative numbers. */'#13#10 +
        'select @Integer = -1000'#13#10 +
        #13#10 +
        'select "Both Signs" ='#13#10 +
        '  substring( "- +", (sign(@Integer) + 2), 1) +'#13#10 +
        '  right(replicate("0",12) + '#13#10 +
        '    convert(varchar, abs(@Integer)),12)'#13#10 +
        #13#10 +
        'select @Integer = 1000'#13#10 +
        #13#10 +
        'select "Both Signs" ='#13#10 +
        '  substring( "- +", (sign(@Integer) + 2), 1) +'#13#10 +
        '  right(replicate("0",12) + '#13#10 +
        '    convert(varchar, abs(@Integer)),12)'#13#10 +
        #13#10 +
        'go';
    sqlIngres:
      Result := '/* Ingres example source */'#13#10 +
        'DELETE'#13#10 +
        'FROM t1'#13#10 +
        'WHERE EXISTS'#13#10 +
        '(SELECT t2.column1, t2.column2'#13#10 +
        'FROM t2'#13#10 +
        'WHERE t1.column1 = t2.column1 and'#13#10 +
        't1.column2 = t2.column2)';
    sqlMSSQL7:
      Result := '/* SQL Server 7 example source */'#13#10 +
        'SET QUOTED_IDENTIFIER ON'#13#10 +
        'GO'#13#10 +
        'SET ANSI_NULLS OFF'#13#10 +
        'GO'#13#10 +
        #13#10 +
        '/* Object:  Stored Procedure dbo.sp_PPQInsertOrder */'#13#10 +
        'CREATE PROCEDURE sp_PPQInsertOrder'#13#10 +
        '  @Name    varchar(25),'#13#10 +
        '  @Address varchar(255),'#13#10 +
        '  @ZipCode varchar(15)'#13#10 +
        'AS'#13#10 +
        '  INSERT INTO PPQOrders(Name, Address, ZipCode, OrderDate)'#13#10 +
        '  VALUES (@Name, @Address, @ZipCode, GetDate())'#13#10 +
        #13#10 +
        '  SELECT SCOPE_IDENTITY()'#13#10 +
        'GO';
    sqlMSSQL2K:
      Result := '/* SQL Server2000 example source */'#13#10 +
        'SET QUOTED_IDENTIFIER ON'#13#10 +
        'GO'#13#10 +
        'SET ANSI_NULLS OFF'#13#10 +
        'GO'#13#10 +
        #13#10 +
        '/* Object:  Stored Procedure dbo.sp_PPQInsertOrder */'#13#10 +
        'CREATE PROCEDURE sp_PPQInsertOrder'#13#10 +
        '  @Name    varchar(25),'#13#10 +
        '  @Address varchar(255),'#13#10 +
        '  @ZipCode varchar(15)'#13#10 +
        'AS'#13#10 +
        '  INSERT INTO PPQOrders(Name, Address, ZipCode, OrderDate)'#13#10 +
        '  VALUES (@Name, @Address, @ZipCode, GetDate())'#13#10 +
        #13#10 +
        '  SELECT SCOPE_IDENTITY()'#13#10 +
        'GO';
  end;
end;

class function TSynSQLSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangSQL;
end;

function TSynSQLSyn.GetKeyWords(TokenKind: Integer): string;
begin
  Result := '';

  case fDialect of
    sqlPostgres:
      begin
        case TtkTokenKind(TokenKind) of
          tkDatatype: Result := PostgresTypes;
          tkKey: Result := PostgresKW;
          tkFunction: Result := PostgresFunctions;
          tkException: Result := PostgresExceptions;
        end;
      end;
    sqlIngres:
      case TtkTokenKind(TokenKind) of
        tkDatatype: Result := IngresTypes;
        tkKey: Result := IngresKW;
        tkFunction: Result := IngresFunctions;
      end;
    sqlInterbase6:
      case TtkTokenKind(TokenKind) of
        tkDatatype: Result := Interbase6Types;
        tkFunction: Result := Interbase6Functions;
        tkKey: Result := Interbase6KW;
      end;
    sqlMSSQL7:
      case TtkTokenKind(TokenKind) of
        tkKey: Result := MSSQL7KW;
        tkDatatype: Result := MSSQL7Types;
        tkFunction: Result := MSSQL7Functions;
      end;
    sqlMSSQL2K:
      case TtkTokenKind(TokenKind) of
        tkKey: Result := MSSQL2000KW;
        tkDataType: Result := MSSQL2000Types;
        tkFunction: Result := MSSQL2000Functions;
      end;
    sqlMySql:
      case TtkTokenKind(TokenKind) of
        tkKey: Result := MySqlKW;
        tkDatatype: Result := MySqlTypes;
        tkFunction: Result := MySqlFunctions;
        tkPLSQL: Result := MySQLPLSQLKW;
      end;
    sqlOracle:
      case TtkTokenKind(TokenKind) of
        tkKey: Result := OracleKW;
        tkDatatype: Result := OracleTypes;
        tkException: Result := OracleExceptions;
        tkFunction: Result := OracleFunctions;
        tkComment: Result := OracleCommentKW;
        tkConsoleOutput: Result := OracleConsoleOutputKW;
        tkDefaultPackage: Result := OracleDefaultPackages;
        tkPLSQL: Result := OraclePLSQLKW;
        tkSQLPlus: Result := OracleSQLPlusCommands;
      end;
    sqlStandard:
      if TtkTokenKind(TokenKind) = tkKey then
        Result := StandardKW;
    sqlSybase:
      if TtkTokenKind(TokenKind) = tkKey then
        Result := SybaseKW;
    sqlNexus:
      case TtkTokenKind(TokenKind) of
        tkKey: Result := NexusKW;
        tkDatatype: Result := NexusTypes;
        tkFunction: Result := NexusFunctions;
      end;
    sqlInformix:
      case TtkTokenKind(TokenKind) of
        tkKey: Result := InformixKW;
        tkDatatype: Result := InformixTypes;
        tkFunction: Result := InformixFunctions;
      end;
  end;
end;

initialization
  RegisterPlaceableHighlighter(TSynSQLSyn);
end.
