module JsonSerializer where

import JSONTypes

-- ����������� JSONValue � ������ � ������� JSON
jsonValueToString :: JSONValue -> String
jsonValueToString = "help"    -- ����������� ������������

-- ��� ����������� JSONObject � ������
jsonObjectToString :: JSONObject -> String
jsonObjectToString (JSONObject pairs) = "help"   -- ����������� ���� "����: ��������"