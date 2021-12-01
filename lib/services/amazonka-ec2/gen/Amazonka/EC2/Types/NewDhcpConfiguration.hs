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
-- Module      : Amazonka.EC2.Types.NewDhcpConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NewDhcpConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newNewDhcpConfiguration' smart constructor.
data NewDhcpConfiguration = NewDhcpConfiguration'
  { values :: Prelude.Maybe [Prelude.Text],
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NewDhcpConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'newDhcpConfiguration_values' - Undocumented member.
--
-- 'key', 'newDhcpConfiguration_key' - Undocumented member.
newNewDhcpConfiguration ::
  NewDhcpConfiguration
newNewDhcpConfiguration =
  NewDhcpConfiguration'
    { values = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | Undocumented member.
newDhcpConfiguration_values :: Lens.Lens' NewDhcpConfiguration (Prelude.Maybe [Prelude.Text])
newDhcpConfiguration_values = Lens.lens (\NewDhcpConfiguration' {values} -> values) (\s@NewDhcpConfiguration' {} a -> s {values = a} :: NewDhcpConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
newDhcpConfiguration_key :: Lens.Lens' NewDhcpConfiguration (Prelude.Maybe Prelude.Text)
newDhcpConfiguration_key = Lens.lens (\NewDhcpConfiguration' {key} -> key) (\s@NewDhcpConfiguration' {} a -> s {key = a} :: NewDhcpConfiguration)

instance Prelude.Hashable NewDhcpConfiguration where
  hashWithSalt salt' NewDhcpConfiguration' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData NewDhcpConfiguration where
  rnf NewDhcpConfiguration' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf key

instance Core.ToQuery NewDhcpConfiguration where
  toQuery NewDhcpConfiguration' {..} =
    Prelude.mconcat
      [ Core.toQuery
          (Core.toQueryList "Value" Prelude.<$> values),
        "Key" Core.=: key
      ]
