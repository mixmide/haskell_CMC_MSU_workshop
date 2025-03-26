module XMLTypes where

-- ���� XML. ��� ������������� ��������� XML-��������� � ���� ������ �����

data XMLNode
  = TextNode String          -- ��������� ����
  | ElementNode
      String                 -- �������� ����
      [(String, String)]     -- ������ ��� "����-��������"
      [XMLNode]              -- �������� ����
  deriving (Show, Eq)