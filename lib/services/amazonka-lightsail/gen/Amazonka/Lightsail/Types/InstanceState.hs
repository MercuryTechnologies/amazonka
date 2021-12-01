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
-- Module      : Amazonka.Lightsail.Types.InstanceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the virtual private server (or /instance/) status.
--
-- /See:/ 'newInstanceState' smart constructor.
data InstanceState = InstanceState'
  { -- | The state of the instance (e.g., @running@ or @pending@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The status code for the instance.
    code :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'instanceState_name' - The state of the instance (e.g., @running@ or @pending@).
--
-- 'code', 'instanceState_code' - The status code for the instance.
newInstanceState ::
  InstanceState
newInstanceState =
  InstanceState'
    { name = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The state of the instance (e.g., @running@ or @pending@).
instanceState_name :: Lens.Lens' InstanceState (Prelude.Maybe Prelude.Text)
instanceState_name = Lens.lens (\InstanceState' {name} -> name) (\s@InstanceState' {} a -> s {name = a} :: InstanceState)

-- | The status code for the instance.
instanceState_code :: Lens.Lens' InstanceState (Prelude.Maybe Prelude.Int)
instanceState_code = Lens.lens (\InstanceState' {code} -> code) (\s@InstanceState' {} a -> s {code = a} :: InstanceState)

instance Core.FromJSON InstanceState where
  parseJSON =
    Core.withObject
      "InstanceState"
      ( \x ->
          InstanceState'
            Prelude.<$> (x Core..:? "name") Prelude.<*> (x Core..:? "code")
      )

instance Prelude.Hashable InstanceState where
  hashWithSalt salt' InstanceState' {..} =
    salt' `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` name

instance Prelude.NFData InstanceState where
  rnf InstanceState' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf code
