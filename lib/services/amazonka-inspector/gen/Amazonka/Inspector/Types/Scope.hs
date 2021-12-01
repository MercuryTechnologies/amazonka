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
-- Module      : Amazonka.Inspector.Types.Scope
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.Scope where

import qualified Amazonka.Core as Core
import Amazonka.Inspector.Types.ScopeType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | This data type contains key-value pairs that identify various Amazon
-- resources.
--
-- /See:/ 'newScope' smart constructor.
data Scope = Scope'
  { -- | The resource identifier for the specified scope type.
    value :: Prelude.Maybe Prelude.Text,
    -- | The type of the scope.
    key :: Prelude.Maybe ScopeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'scope_value' - The resource identifier for the specified scope type.
--
-- 'key', 'scope_key' - The type of the scope.
newScope ::
  Scope
newScope =
  Scope'
    { value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The resource identifier for the specified scope type.
scope_value :: Lens.Lens' Scope (Prelude.Maybe Prelude.Text)
scope_value = Lens.lens (\Scope' {value} -> value) (\s@Scope' {} a -> s {value = a} :: Scope)

-- | The type of the scope.
scope_key :: Lens.Lens' Scope (Prelude.Maybe ScopeType)
scope_key = Lens.lens (\Scope' {key} -> key) (\s@Scope' {} a -> s {key = a} :: Scope)

instance Core.FromJSON Scope where
  parseJSON =
    Core.withObject
      "Scope"
      ( \x ->
          Scope'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "key")
      )

instance Prelude.Hashable Scope where
  hashWithSalt salt' Scope' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Scope where
  rnf Scope' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key
