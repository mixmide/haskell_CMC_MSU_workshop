module JSONTypes where

-- JSON-��������: ������, ������, ������ (���������)

data JSONValue
  = JSONString String        -- ������
  | JSONObject JSONObject    -- ������
  | JSONArray [JSONValue]    -- ������ ��������
  deriving (Show, Eq)

-- JSON-������ - ������ ��� "����-��������"

newtype JSONObject = JSONObject [(String, JSONValue)]
  deriving (Show, Eq)