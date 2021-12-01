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
-- Module      : Amazonka.IoTWireless.Types.SessionKeysAbpV1_1
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SessionKeysAbpV1_1 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Session keys for ABP v1.1
--
-- /See:/ 'newSessionKeysAbpV1_1' smart constructor.
data SessionKeysAbpV1_1 = SessionKeysAbpV1_1'
  { -- | The FNwkSIntKey value.
    fNwkSIntKey :: Prelude.Maybe Prelude.Text,
    -- | The SNwkSIntKey value.
    sNwkSIntKey :: Prelude.Maybe Prelude.Text,
    -- | The NwkSEncKey value.
    nwkSEncKey :: Prelude.Maybe Prelude.Text,
    -- | The AppSKey value.
    appSKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionKeysAbpV1_1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fNwkSIntKey', 'sessionKeysAbpV1_1_fNwkSIntKey' - The FNwkSIntKey value.
--
-- 'sNwkSIntKey', 'sessionKeysAbpV1_1_sNwkSIntKey' - The SNwkSIntKey value.
--
-- 'nwkSEncKey', 'sessionKeysAbpV1_1_nwkSEncKey' - The NwkSEncKey value.
--
-- 'appSKey', 'sessionKeysAbpV1_1_appSKey' - The AppSKey value.
newSessionKeysAbpV1_1 ::
  SessionKeysAbpV1_1
newSessionKeysAbpV1_1 =
  SessionKeysAbpV1_1'
    { fNwkSIntKey = Prelude.Nothing,
      sNwkSIntKey = Prelude.Nothing,
      nwkSEncKey = Prelude.Nothing,
      appSKey = Prelude.Nothing
    }

-- | The FNwkSIntKey value.
sessionKeysAbpV1_1_fNwkSIntKey :: Lens.Lens' SessionKeysAbpV1_1 (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_1_fNwkSIntKey = Lens.lens (\SessionKeysAbpV1_1' {fNwkSIntKey} -> fNwkSIntKey) (\s@SessionKeysAbpV1_1' {} a -> s {fNwkSIntKey = a} :: SessionKeysAbpV1_1)

-- | The SNwkSIntKey value.
sessionKeysAbpV1_1_sNwkSIntKey :: Lens.Lens' SessionKeysAbpV1_1 (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_1_sNwkSIntKey = Lens.lens (\SessionKeysAbpV1_1' {sNwkSIntKey} -> sNwkSIntKey) (\s@SessionKeysAbpV1_1' {} a -> s {sNwkSIntKey = a} :: SessionKeysAbpV1_1)

-- | The NwkSEncKey value.
sessionKeysAbpV1_1_nwkSEncKey :: Lens.Lens' SessionKeysAbpV1_1 (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_1_nwkSEncKey = Lens.lens (\SessionKeysAbpV1_1' {nwkSEncKey} -> nwkSEncKey) (\s@SessionKeysAbpV1_1' {} a -> s {nwkSEncKey = a} :: SessionKeysAbpV1_1)

-- | The AppSKey value.
sessionKeysAbpV1_1_appSKey :: Lens.Lens' SessionKeysAbpV1_1 (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_1_appSKey = Lens.lens (\SessionKeysAbpV1_1' {appSKey} -> appSKey) (\s@SessionKeysAbpV1_1' {} a -> s {appSKey = a} :: SessionKeysAbpV1_1)

instance Core.FromJSON SessionKeysAbpV1_1 where
  parseJSON =
    Core.withObject
      "SessionKeysAbpV1_1"
      ( \x ->
          SessionKeysAbpV1_1'
            Prelude.<$> (x Core..:? "FNwkSIntKey")
            Prelude.<*> (x Core..:? "SNwkSIntKey")
            Prelude.<*> (x Core..:? "NwkSEncKey")
            Prelude.<*> (x Core..:? "AppSKey")
      )

instance Prelude.Hashable SessionKeysAbpV1_1 where
  hashWithSalt salt' SessionKeysAbpV1_1' {..} =
    salt' `Prelude.hashWithSalt` appSKey
      `Prelude.hashWithSalt` nwkSEncKey
      `Prelude.hashWithSalt` sNwkSIntKey
      `Prelude.hashWithSalt` fNwkSIntKey

instance Prelude.NFData SessionKeysAbpV1_1 where
  rnf SessionKeysAbpV1_1' {..} =
    Prelude.rnf fNwkSIntKey
      `Prelude.seq` Prelude.rnf appSKey
      `Prelude.seq` Prelude.rnf nwkSEncKey
      `Prelude.seq` Prelude.rnf sNwkSIntKey

instance Core.ToJSON SessionKeysAbpV1_1 where
  toJSON SessionKeysAbpV1_1' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FNwkSIntKey" Core..=) Prelude.<$> fNwkSIntKey,
            ("SNwkSIntKey" Core..=) Prelude.<$> sNwkSIntKey,
            ("NwkSEncKey" Core..=) Prelude.<$> nwkSEncKey,
            ("AppSKey" Core..=) Prelude.<$> appSKey
          ]
      )
