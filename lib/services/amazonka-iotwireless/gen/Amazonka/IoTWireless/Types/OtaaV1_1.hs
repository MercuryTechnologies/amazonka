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
-- Module      : Amazonka.IoTWireless.Types.OtaaV1_1
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.OtaaV1_1 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | OTAA device object for v1.1
--
-- /See:/ 'newOtaaV1_1' smart constructor.
data OtaaV1_1 = OtaaV1_1'
  { -- | The JoinEUI value.
    joinEui :: Prelude.Maybe Prelude.Text,
    -- | The NwkKey value.
    nwkKey :: Prelude.Maybe Prelude.Text,
    -- | The AppKey value.
    appKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OtaaV1_1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'joinEui', 'otaaV1_1_joinEui' - The JoinEUI value.
--
-- 'nwkKey', 'otaaV1_1_nwkKey' - The NwkKey value.
--
-- 'appKey', 'otaaV1_1_appKey' - The AppKey value.
newOtaaV1_1 ::
  OtaaV1_1
newOtaaV1_1 =
  OtaaV1_1'
    { joinEui = Prelude.Nothing,
      nwkKey = Prelude.Nothing,
      appKey = Prelude.Nothing
    }

-- | The JoinEUI value.
otaaV1_1_joinEui :: Lens.Lens' OtaaV1_1 (Prelude.Maybe Prelude.Text)
otaaV1_1_joinEui = Lens.lens (\OtaaV1_1' {joinEui} -> joinEui) (\s@OtaaV1_1' {} a -> s {joinEui = a} :: OtaaV1_1)

-- | The NwkKey value.
otaaV1_1_nwkKey :: Lens.Lens' OtaaV1_1 (Prelude.Maybe Prelude.Text)
otaaV1_1_nwkKey = Lens.lens (\OtaaV1_1' {nwkKey} -> nwkKey) (\s@OtaaV1_1' {} a -> s {nwkKey = a} :: OtaaV1_1)

-- | The AppKey value.
otaaV1_1_appKey :: Lens.Lens' OtaaV1_1 (Prelude.Maybe Prelude.Text)
otaaV1_1_appKey = Lens.lens (\OtaaV1_1' {appKey} -> appKey) (\s@OtaaV1_1' {} a -> s {appKey = a} :: OtaaV1_1)

instance Core.FromJSON OtaaV1_1 where
  parseJSON =
    Core.withObject
      "OtaaV1_1"
      ( \x ->
          OtaaV1_1'
            Prelude.<$> (x Core..:? "JoinEui")
            Prelude.<*> (x Core..:? "NwkKey")
            Prelude.<*> (x Core..:? "AppKey")
      )

instance Prelude.Hashable OtaaV1_1 where
  hashWithSalt salt' OtaaV1_1' {..} =
    salt' `Prelude.hashWithSalt` appKey
      `Prelude.hashWithSalt` nwkKey
      `Prelude.hashWithSalt` joinEui

instance Prelude.NFData OtaaV1_1 where
  rnf OtaaV1_1' {..} =
    Prelude.rnf joinEui
      `Prelude.seq` Prelude.rnf appKey
      `Prelude.seq` Prelude.rnf nwkKey

instance Core.ToJSON OtaaV1_1 where
  toJSON OtaaV1_1' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("JoinEui" Core..=) Prelude.<$> joinEui,
            ("NwkKey" Core..=) Prelude.<$> nwkKey,
            ("AppKey" Core..=) Prelude.<$> appKey
          ]
      )
