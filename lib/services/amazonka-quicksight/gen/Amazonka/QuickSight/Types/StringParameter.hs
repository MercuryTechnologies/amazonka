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
-- Module      : Amazonka.QuickSight.Types.StringParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.StringParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A string parameter.
--
-- /See:/ 'newStringParameter' smart constructor.
data StringParameter = StringParameter'
  { -- | A display name for a string parameter.
    name :: Prelude.Text,
    -- | The values of a string parameter.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stringParameter_name' - A display name for a string parameter.
--
-- 'values', 'stringParameter_values' - The values of a string parameter.
newStringParameter ::
  -- | 'name'
  Prelude.Text ->
  StringParameter
newStringParameter pName_ =
  StringParameter'
    { name = pName_,
      values = Prelude.mempty
    }

-- | A display name for a string parameter.
stringParameter_name :: Lens.Lens' StringParameter Prelude.Text
stringParameter_name = Lens.lens (\StringParameter' {name} -> name) (\s@StringParameter' {} a -> s {name = a} :: StringParameter)

-- | The values of a string parameter.
stringParameter_values :: Lens.Lens' StringParameter [Prelude.Text]
stringParameter_values = Lens.lens (\StringParameter' {values} -> values) (\s@StringParameter' {} a -> s {values = a} :: StringParameter) Prelude.. Lens.coerced

instance Prelude.Hashable StringParameter where
  hashWithSalt salt' StringParameter' {..} =
    salt' `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` name

instance Prelude.NFData StringParameter where
  rnf StringParameter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Core.ToJSON StringParameter where
  toJSON StringParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Values" Core..= values)
          ]
      )
