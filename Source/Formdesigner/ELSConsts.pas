{*******************************************************}
{                                                       }
{       Extension Library                               }
{       Constants Unit                                  }
{                                                       }
{       (c) 1999 - 2001, Balabuyev Yevgeny              }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit ELSConsts;

interface

const
  SELComponentPage = 'Extension Lib';

resourcestring
  SELDsgnrControlLockedDel = 'Control "%s" or some of its children can ' +
    'not' + #13 + ' be deleted because they are locked.';
  SELDsgnrControlsLockedDel = 'Some controls are locked. Can not delete' +
    ' controls.';
  SELDsgnrClipboardFormat = 'Extension Library designer components';

implementation

end.
