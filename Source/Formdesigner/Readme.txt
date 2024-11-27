Library 2 (REFINED BY DAGEEK)
___________________


���������� � ����������
_______________________

������ 2

  * ��������� ��������� ���������� ( �������� ��. ����):
      TELPropertyInspector,
      TELDiagram,
      TELDBDiagram;

  * ���������� ��������� ������.

������ 2.1

  * ��������� �������� � TELPropertyInspector, � TELDesignPanel (��������
      DesignControl �� ����� ��������� �������� TELDesignPanel), � TELDiagram,
      TELDBDiagram (�������� ��� ����������� item'�� � ��������� �� ��������);
  * � TELPropertyInspector ���������� ���������� (�������� ������ ����������
      ���������� ������, ��� � Delphi);
  * � TELDesigner ��������� ���, ��������� � ���, ��� ��� ��������� ��������
      ����������� ���������� ��������� ���������� �� ��� �� �����);
  * � TELDesigner ��������� ��������� "Drag and drop" (������� OnDragOver,
      OnDragDrop, ��. ������ TELDesigner � ����������� \Demos);
  * � TELDesigner �������������� ���������� �����
  * � TELDiagram, TELDBDiagram ���������� ��������� "Drag and drop". ������
      ������������� ����� Item'� ��� � TListView
  * � TELDiagram, TELDBDiagram ��������� ������ � ��������:
      property Data: Pointer; (TELDiagramLink)
      property Data: Pointer; (TELDiagramItem)
      function ItemRect(AIndex: Integer): TRect; (TELDiagramItems)
      property AutoItemPos; (TELCustomDiagram)
      property OnInsertItem; (TELCustomDiagram)
      property OnDeleteItem; (TELCustomDiagram)
      property OnInsertLink; (TELCustomDiagram)
      property OnDeleteLink; (TELCustomDiagram)
      function LineAtPos(APos: TPoint; AExisting: Boolean): Integer; (TELDBDiagramItem)
  * � TELDBDiagram ��������� ��������� OwnerDraw ��� Item'�� (�������� ItemStyle,
      ������� OnMeasureItemLine, OnDrawItemLine)
                      
������ 2.2
  * REFINED BY DAGEEK

����������
__________

��� ���������:
  � ���� Delphi �������� File|Open;
  �������� ���� ExtLib_D6.dpk (��� Delphi 6) ��� ExtLib_D5.dpk (��� Delphi 5);
  � ����������� ���� ������� ������ Install;
  � ���� Delphi �������� Tools|Invironment options;
  � ����������� ���� �������� ������� Library;
  �������� � Library path ������� ����������.

�������� ����������� � �������
______________________________


������ ELControls.

TELEvent, TELEventSender

  ���������� ��������� ������������ ����������� � ������ "���� �� ������".
  � ���������� TELEventSender ����� ���� ���������� ��������� �����������
  TELevent. ��� ������ ������ TELEventsender.SendEvent ��� ��� ��������
  ��������� � ���������� ������� OnEvent. ���������� ����� �������������� 
  ��� ����������� �������� ���������, ������ ��� ������, ����������� ���������
  �� ������� �� ���������� �������, �� �����������. ��. ������ TELEvent �
  ����������� \Demos.

TELInstanceChecker

  ������ ��������� ������������ ������������� ������� ������ ����� ����������
  � �������, � ����� �������� ��� ���������� ����� ���������� �����-���� ������.
  �������, �� ����� �������������� ��� MDI ���������� ���������, ������� ��� 
  ������� (��� ������ �� ���������� �����) �� ��������� ������ ����� ���������,
  � �������� ��������� ������� (��� �����) ��� ����������. ��. ������� CDPlayer
  � TELInstanceChecker � ����������� \Demos.

TELStringList

  ��������� ������� ��� ������ TStringList, ������� ����� �������������� � 
  "design-time".

TELTrayIcon

  ��������� ���������:
    ���������� ������ � system tray;
    ����������� �� ������� ���� ���� ������;
    ���������� ����������� ����;
    �������� ������ ���������� �� ������ �����;
    ������������ �������� ������.


������ ELDsgnr.

TELDesigner
  
  ��������� ��������� �������� � ���������� ����������� �����������
  ���������. �� ����� ��������������� �� ����������� ��������� � 
  ���������� ��������� Delphi. ��������� ����� ��� �����, ��� � 
  ��������� ���������� (�������� TPanel). ����� ������ ��������� ����� 
  ��������� ��� ���������� ��������� �������.

  ��������� �����������:
    ����������� ����� (����� ��������� ������������);
    ��������� ����������� ����������� ���� Hint ��� ��������� �������� 
      �����������, ��� ������� ������ ����������, ��� ��������� �������
      ���� �� ���������;
    ����� SnapToGrid;
    ����������� ������������ ����;
    ��������� ����������� (������ ���������: lmNoMove, lmNoResize, lmNoDelete,
      lmNoInsertIn, lmNoCopy, lmCustom1, lmCustom2, lmCustom3, lmCustom4,
      lmCustom5, lmCustom6, lmCustom7, lmCustom8);
    �������� � ������� ������ (����������� ����������� � ����� ������ ���� ����������
      ����������, ����������� ������� �� � ����������� ������������������);
    ���������� ��������� �������� ��� ����������� ������������: BringToFront, SendToBack,
      AlignToGrid, Align (��������� �����);
    ������� ��������� ��������� (����� ��������������, ��������� �������).
  
  ��. ������� TELDesigner � ReportDesigner �� ����������� \Demos.

TELDesignPanel

  ��������� ������������ ��������� � ����������� TELDesigner ��� �������
  ����������� �� ���������� ������������ TCustomForm. ��. ������ ReportDesigner 
  �� ����������� \Demos.


������ ELUtils.

������� ELPackStrings � ELUnpackStrings
  
  ������� ��������� ������������� TStrings � ������ �
  �������. ��. ������ StreamingStrings �� ����������� \Demos.

������� ELSaveStringToStream � ELLoadStringFromStream

  ������� ��������� ���������� � ������ ������ �� TStream.
  ��. ������ StreamingStrings �� ����������� \Demos.

������� ELPrepareMask � ELMaskCompare

  ������� ��������� ������������ ��������� ������ � ������.
  ����� ����� ������������ WinCard ������� "*" � "?".
  �� ���������� ������� �������� ������� �������, ���
  ����������� ����� Delphi - TMask (� ��������� �������
  ������� �� ������� ���������� ��������� ������������ 100 ���).

������� ELQuickSort

  ���������� ��������� Quick sort ��� ������������� ���� ������.

������� ELSearchFiles

  ����� ������ (� ������� ����������� ��������).

����� TELThreadFilesSearch

  ���� ����� ������, �� � ��������� ������.

������ ELPropInsp.

TELPropertyInspector

  ��������� �������� �������� Delphi ObjectInspector

  ��������� �����������:
    ��������� ���������� � ������������� �������� ������������ � 
      ���� �������� (������ ��������);
    ��� ������� - ������ �� ���������� (Component reference)
      ����� �������, ����������� ������������ ������ ��������� 
      �����������,�������� ������ �� ����� ��� ��� �� ������;
    ��������� ���������� �������� ������ �� ���������� 
      (��� � Delphi 6);
    ��� ����������� ������������ ������� ������� ��������
      PropKinds, � ����� ������� OnFilterProp
    ����� ���������� ��������� ������� ��� ����������� �����
      Objects Pascal, � ����� ��� �������� ����� VCL:
        * TCaption
        * TCursor
        * TFontCharset
        * TFontName
        * TImeName
        * TFont
        * TModalResult
        * TPenStyle
        * TBrushStyle
        * TTabOrder
        * TShortCat
    � ������ ��� �� �������� ����� TELPropEditor, ����������
      ������� ������� ��� �������� ������ ���������� �������,
      ������� ����� ����� ���� ���������������� � ����������
  

������ ELDgrm.

TELDiagram
  
  ��������� ��������� ���������, ������� Item'� (������) �
  ����� ����� ����. ����� ������� ��� ���������� Item'�
  � ������

TELDBDiagram
  
  ������ ��������� ���� ������ MS Access

__________________________________

(c) 1999 - 2002, Balabuyev Yevgeny
E-mail: stalcer@rambler.ru











