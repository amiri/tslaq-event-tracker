WITH RECURSIVE categories AS (
  SELECT
    id,
    name,
    parent_id,
    CAST(NULL AS varchar) AS parents,
    name AS full_name
  FROM
    category
  WHERE
    parent_id IS NULL
  UNION ALL
  SELECT
    subcategories.id,
    subcategories.name,
    subcategories.parent_id,
    CASE WHEN parent_categories.parents IS NULL THEN
      subcategories.parent_id::varchar
    ELSE
      (parent_categories.parents || ' > ' || subcategories.parent_id)
    END AS parents,
    (parent_categories.full_name || ' > ' || subcategories.name) AS full_name
  FROM
    category AS subcategories
    INNER JOIN categories AS parent_categories ON (subcategories.parent_id = parent_categories.id))
SELECT
  id,
  name,
  parent_id,
  parents,
  full_name
FROM
  categories
ORDER BY
  full_name;
