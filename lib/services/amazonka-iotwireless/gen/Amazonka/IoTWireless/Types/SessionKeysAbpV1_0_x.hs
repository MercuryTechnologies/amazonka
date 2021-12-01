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
-- Module      : Amazonka.IoTWireless.Types.SessionKeysAbpV1_0_x
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SessionKeysAbpV1_0_x where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Session keys for ABP v1.1
--
-- /See:/ 'newSessionKeysAbpV1_0_x' smart constructor.
data SessionKeysAbpV1_0_x = SessionKeysAbpV1_0_x'
  { -- | The NwkSKey value.
    nwkSKey :: Prelude.Maybe Prelude.Text,
    -- | The AppSKey value.
    appSKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionKeysAbpV1_0_x' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nwkSKey', 'sessionKeysAbpV1_0_x_nwkSKey' - The NwkSKey value.
--
-- 'appSKey', 'sessionKeysAbpV1_0_x_appSKey' - The AppSKey value.
newSessionKeysAbpV1_0_x ::
  SessionKeysAbpV1_0_x
newSessionKeysAbpV1_0_x =
  SessionKeysAbpV1_0_x'
    { nwkSKey = Prelude.Nothing,
      appSKey = Prelude.Nothing
    }

-- | The NwkSKey value.
sessionKeysAbpV1_0_x_nwkSKey :: Lens.Lens' SessionKeysAbpV1_0_x (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_0_x_nwkSKey = Lens.lens (\SessionKeysAbpV1_0_x' {nwkSKey} -> nwkSKey) (\s@SessionKeysAbpV1_0_x' {} a -> s {nwkSKey = a} :: SessionKeysAbpV1_0_x)

-- | The AppSKey value.
sessionKeysAbpV1_0_x_appSKey :: Lens.Lens' SessionKeysAbpV1_0_x (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_0_x_appSKey = Lens.lens (\SessionKeysAbpV1_0_x' {appSKey} -> appSKey) (\s@SessionKeysAbpV1_0_x' {} a -> s {appSKey = a} :: SessionKeysAbpV1_0_x)

instance Core.FromJSON SessionKeysAbpV1_0_x where
  parseJSON =
    Core.withObject
      "SessionKeysAbpV1_0_x"
      ( \x ->
          SessionKeysAbpV1_0_x'
            Prelude.<$> (x Core..:? "NwkSKey")
            Prelude.<*> (x Core..:? "AppSKey")
      )

instance Prelude.Hashable SessionKeysAbpV1_0_x where
  hashWithSalt salt' SessionKeysAbpV1_0_x' {..} =
    salt' `Prelude.hashWithSalt` appSKey
      `Prelude.hashWithSalt` nwkSKey

instance Prelude.NFData SessionKeysAbpV1_0_x where
  rnf SessionKeysAbpV1_0_x' {..} =
    Prelude.rnf nwkSKey
      `Prelude.seq` Prelude.rnf appSKey

instance Core.ToJSON SessionKeysAbpV1_0_x where
  toJSON SessionKeysAbpV1_0_x' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NwkSKey" Core..=) Prelude.<$> nwkSKey,
            ("AppSKey" Core..=) Prelude.<$> appSKey
          ]
      )
