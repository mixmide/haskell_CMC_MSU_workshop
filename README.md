# data-project

## Конвертер XML в JSON на языке Haskell
Это программа на языке Haskell для преобразования XML-файлов в JSON формат. Принимает входной XML-файл и выходной JSON-файл через командную строку в качестве параметров.

### Внутреннее представление
- **XML**: XMLNode — текст (TextNode) или элемент с атрибутами и дочерними узлами (ElementNode)l;
- **JSON**: JSONValue — строка (JSONString), объект (JSONObj) или массив (JSONArray).

### Модули
- **XMLTypes.hs**: тип XMLNode
- **JSONTypes.hs**: типы JSONValue и JSONObject
- **XmlParser.hs**: парсинг XML в XMLNode
- **JsonConverter.hs**: преобразование XMLNode в JSONValue
- **JsonSerializer.hs**: сериализация JSONValue в строку
- **Main.hs**: основная функция (чтение => преобразование => запись)

### Основные функции
- **parseXml**: парсит XML в XMLNode.
- **xmlToJson**: преобразует XMLNode в JSONValue.
- **jsonValueToString**: преобразует JSONValue в строку.

### Запуск
_PowerShell_:
- stack build
- stack exec -- data-project-exe input.xml output.json
