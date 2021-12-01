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
-- Module      : Amazonka.IoTWireless.Types.SidewalkAccountInfoWithFingerprint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkAccountInfoWithFingerprint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a Sidewalk account.
--
-- /See:/ 'newSidewalkAccountInfoWithFingerprint' smart constructor.
data SidewalkAccountInfoWithFingerprint = SidewalkAccountInfoWithFingerprint'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The fingerprint of the Sidewalk application server private key.
    fingerprint :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Sidewalk Amazon ID.
    amazonId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkAccountInfoWithFingerprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'sidewalkAccountInfoWithFingerprint_arn' - The Amazon Resource Name of the resource.
--
-- 'fingerprint', 'sidewalkAccountInfoWithFingerprint_fingerprint' - The fingerprint of the Sidewalk application server private key.
--
-- 'amazonId', 'sidewalkAccountInfoWithFingerprint_amazonId' - The Sidewalk Amazon ID.
newSidewalkAccountInfoWithFingerprint ::
  SidewalkAccountInfoWithFingerprint
newSidewalkAccountInfoWithFingerprint =
  SidewalkAccountInfoWithFingerprint'
    { arn =
        Prelude.Nothing,
      fingerprint = Prelude.Nothing,
      amazonId = Prelude.Nothing
    }

-- | The Amazon Resource Name of the resource.
sidewalkAccountInfoWithFingerprint_arn :: Lens.Lens' SidewalkAccountInfoWithFingerprint (Prelude.Maybe Prelude.Text)
sidewalkAccountInfoWithFingerprint_arn = Lens.lens (\SidewalkAccountInfoWithFingerprint' {arn} -> arn) (\s@SidewalkAccountInfoWithFingerprint' {} a -> s {arn = a} :: SidewalkAccountInfoWithFingerprint)

-- | The fingerprint of the Sidewalk application server private key.
sidewalkAccountInfoWithFingerprint_fingerprint :: Lens.Lens' SidewalkAccountInfoWithFingerprint (Prelude.Maybe Prelude.Text)
sidewalkAccountInfoWithFingerprint_fingerprint = Lens.lens (\SidewalkAccountInfoWithFingerprint' {fingerprint} -> fingerprint) (\s@SidewalkAccountInfoWithFingerprint' {} a -> s {fingerprint = a} :: SidewalkAccountInfoWithFingerprint) Prelude.. Lens.mapping Core._Sensitive

-- | The Sidewalk Amazon ID.
sidewalkAccountInfoWithFingerprint_amazonId :: Lens.Lens' SidewalkAccountInfoWithFingerprint (Prelude.Maybe Prelude.Text)
sidewalkAccountInfoWithFingerprint_amazonId = Lens.lens (\SidewalkAccountInfoWithFingerprint' {amazonId} -> amazonId) (\s@SidewalkAccountInfoWithFingerprint' {} a -> s {amazonId = a} :: SidewalkAccountInfoWithFingerprint)

instance
  Core.FromJSON
    SidewalkAccountInfoWithFingerprint
  where
  parseJSON =
    Core.withObject
      "SidewalkAccountInfoWithFingerprint"
      ( \x ->
          SidewalkAccountInfoWithFingerprint'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Fingerprint")
            Prelude.<*> (x Core..:? "AmazonId")
      )

instance
  Prelude.Hashable
    SidewalkAccountInfoWithFingerprint
  where
  hashWithSalt
    salt'
    SidewalkAccountInfoWithFingerprint' {..} =
      salt' `Prelude.hashWithSalt` amazonId
        `Prelude.hashWithSalt` fingerprint
        `Prelude.hashWithSalt` arn

instance
  Prelude.NFData
    SidewalkAccountInfoWithFingerprint
  where
  rnf SidewalkAccountInfoWithFingerprint' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf amazonId
      `Prelude.seq` Prelude.rnf fingerprint
