{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDSData.Types.SqlParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDSData.Types.SqlParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDSData.Types.Field
import Amazonka.RDSData.Types.TypeHint

-- | A parameter used in a SQL statement.
--
-- /See:/ 'newSqlParameter' smart constructor.
data SqlParameter = SqlParameter'
  { -- | The value of the parameter.
    value :: Prelude.Maybe Field,
    -- | The name of the parameter.
    name :: Prelude.Maybe Prelude.Text,
    -- | A hint that specifies the correct object type for data type mapping.
    -- Possible values are as follows:
    --
    -- -   @DATE@ - The corresponding @String@ parameter value is sent as an
    --     object of @DATE@ type to the database. The accepted format is
    --     @YYYY-MM-DD@.
    --
    -- -   @DECIMAL@ - The corresponding @String@ parameter value is sent as an
    --     object of @DECIMAL@ type to the database.
    --
    -- -   @JSON@ - The corresponding @String@ parameter value is sent as an
    --     object of @JSON@ type to the database.
    --
    -- -   @TIME@ - The corresponding @String@ parameter value is sent as an
    --     object of @TIME@ type to the database. The accepted format is
    --     @HH:MM:SS[.FFF]@.
    --
    -- -   @TIMESTAMP@ - The corresponding @String@ parameter value is sent as
    --     an object of @TIMESTAMP@ type to the database. The accepted format
    --     is @YYYY-MM-DD HH:MM:SS[.FFF]@.
    --
    -- -   @UUID@ - The corresponding @String@ parameter value is sent as an
    --     object of @UUID@ type to the database.
    typeHint :: Prelude.Maybe TypeHint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqlParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'sqlParameter_value' - The value of the parameter.
--
-- 'name', 'sqlParameter_name' - The name of the parameter.
--
-- 'typeHint', 'sqlParameter_typeHint' - A hint that specifies the correct object type for data type mapping.
-- Possible values are as follows:
--
-- -   @DATE@ - The corresponding @String@ parameter value is sent as an
--     object of @DATE@ type to the database. The accepted format is
--     @YYYY-MM-DD@.
--
-- -   @DECIMAL@ - The corresponding @String@ parameter value is sent as an
--     object of @DECIMAL@ type to the database.
--
-- -   @JSON@ - The corresponding @String@ parameter value is sent as an
--     object of @JSON@ type to the database.
--
-- -   @TIME@ - The corresponding @String@ parameter value is sent as an
--     object of @TIME@ type to the database. The accepted format is
--     @HH:MM:SS[.FFF]@.
--
-- -   @TIMESTAMP@ - The corresponding @String@ parameter value is sent as
--     an object of @TIMESTAMP@ type to the database. The accepted format
--     is @YYYY-MM-DD HH:MM:SS[.FFF]@.
--
-- -   @UUID@ - The corresponding @String@ parameter value is sent as an
--     object of @UUID@ type to the database.
newSqlParameter ::
  SqlParameter
newSqlParameter =
  SqlParameter'
    { value = Prelude.Nothing,
      name = Prelude.Nothing,
      typeHint = Prelude.Nothing
    }

-- | The value of the parameter.
sqlParameter_value :: Lens.Lens' SqlParameter (Prelude.Maybe Field)
sqlParameter_value = Lens.lens (\SqlParameter' {value} -> value) (\s@SqlParameter' {} a -> s {value = a} :: SqlParameter)

-- | The name of the parameter.
sqlParameter_name :: Lens.Lens' SqlParameter (Prelude.Maybe Prelude.Text)
sqlParameter_name = Lens.lens (\SqlParameter' {name} -> name) (\s@SqlParameter' {} a -> s {name = a} :: SqlParameter)

-- | A hint that specifies the correct object type for data type mapping.
-- Possible values are as follows:
--
-- -   @DATE@ - The corresponding @String@ parameter value is sent as an
--     object of @DATE@ type to the database. The accepted format is
--     @YYYY-MM-DD@.
--
-- -   @DECIMAL@ - The corresponding @String@ parameter value is sent as an
--     object of @DECIMAL@ type to the database.
--
-- -   @JSON@ - The corresponding @String@ parameter value is sent as an
--     object of @JSON@ type to the database.
--
-- -   @TIME@ - The corresponding @String@ parameter value is sent as an
--     object of @TIME@ type to the database. The accepted format is
--     @HH:MM:SS[.FFF]@.
--
-- -   @TIMESTAMP@ - The corresponding @String@ parameter value is sent as
--     an object of @TIMESTAMP@ type to the database. The accepted format
--     is @YYYY-MM-DD HH:MM:SS[.FFF]@.
--
-- -   @UUID@ - The corresponding @String@ parameter value is sent as an
--     object of @UUID@ type to the database.
sqlParameter_typeHint :: Lens.Lens' SqlParameter (Prelude.Maybe TypeHint)
sqlParameter_typeHint = Lens.lens (\SqlParameter' {typeHint} -> typeHint) (\s@SqlParameter' {} a -> s {typeHint = a} :: SqlParameter)

instance Prelude.Hashable SqlParameter where
  hashWithSalt salt' SqlParameter' {..} =
    salt' `Prelude.hashWithSalt` typeHint
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData SqlParameter where
  rnf SqlParameter' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf typeHint
      `Prelude.seq` Prelude.rnf name

instance Core.ToJSON SqlParameter where
  toJSON SqlParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("name" Core..=) Prelude.<$> name,
            ("typeHint" Core..=) Prelude.<$> typeHint
          ]
      )
