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
-- Module      : Amazonka.Athena.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.EncryptionConfiguration where

import Amazonka.Athena.Types.EncryptionOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | If query results are encrypted in Amazon S3, indicates the encryption
-- option used (for example, @SSE-KMS@ or @CSE-KMS@) and key information.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | For @SSE-KMS@ and @CSE-KMS@, this is the KMS key ARN or ID.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Amazon S3 server-side encryption with Amazon
    -- S3-managed keys (@SSE-S3@), server-side encryption with KMS-managed keys
    -- (@SSE-KMS@), or client-side encryption with KMS-managed keys (CSE-KMS)
    -- is used.
    --
    -- If a query runs in a workgroup and the workgroup overrides client-side
    -- settings, then the workgroup\'s setting for encryption is used. It
    -- specifies whether query results must be encrypted, for all queries that
    -- run in this workgroup.
    encryptionOption :: EncryptionOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'encryptionConfiguration_kmsKey' - For @SSE-KMS@ and @CSE-KMS@, this is the KMS key ARN or ID.
--
-- 'encryptionOption', 'encryptionConfiguration_encryptionOption' - Indicates whether Amazon S3 server-side encryption with Amazon
-- S3-managed keys (@SSE-S3@), server-side encryption with KMS-managed keys
-- (@SSE-KMS@), or client-side encryption with KMS-managed keys (CSE-KMS)
-- is used.
--
-- If a query runs in a workgroup and the workgroup overrides client-side
-- settings, then the workgroup\'s setting for encryption is used. It
-- specifies whether query results must be encrypted, for all queries that
-- run in this workgroup.
newEncryptionConfiguration ::
  -- | 'encryptionOption'
  EncryptionOption ->
  EncryptionConfiguration
newEncryptionConfiguration pEncryptionOption_ =
  EncryptionConfiguration'
    { kmsKey = Prelude.Nothing,
      encryptionOption = pEncryptionOption_
    }

-- | For @SSE-KMS@ and @CSE-KMS@, this is the KMS key ARN or ID.
encryptionConfiguration_kmsKey :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe Prelude.Text)
encryptionConfiguration_kmsKey = Lens.lens (\EncryptionConfiguration' {kmsKey} -> kmsKey) (\s@EncryptionConfiguration' {} a -> s {kmsKey = a} :: EncryptionConfiguration)

-- | Indicates whether Amazon S3 server-side encryption with Amazon
-- S3-managed keys (@SSE-S3@), server-side encryption with KMS-managed keys
-- (@SSE-KMS@), or client-side encryption with KMS-managed keys (CSE-KMS)
-- is used.
--
-- If a query runs in a workgroup and the workgroup overrides client-side
-- settings, then the workgroup\'s setting for encryption is used. It
-- specifies whether query results must be encrypted, for all queries that
-- run in this workgroup.
encryptionConfiguration_encryptionOption :: Lens.Lens' EncryptionConfiguration EncryptionOption
encryptionConfiguration_encryptionOption = Lens.lens (\EncryptionConfiguration' {encryptionOption} -> encryptionOption) (\s@EncryptionConfiguration' {} a -> s {encryptionOption = a} :: EncryptionConfiguration)

instance Core.FromJSON EncryptionConfiguration where
  parseJSON =
    Core.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Core..:? "KmsKey")
            Prelude.<*> (x Core..: "EncryptionOption")
      )

instance Prelude.Hashable EncryptionConfiguration where
  hashWithSalt salt' EncryptionConfiguration' {..} =
    salt' `Prelude.hashWithSalt` encryptionOption
      `Prelude.hashWithSalt` kmsKey

instance Prelude.NFData EncryptionConfiguration where
  rnf EncryptionConfiguration' {..} =
    Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf encryptionOption

instance Core.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsKey" Core..=) Prelude.<$> kmsKey,
            Prelude.Just
              ("EncryptionOption" Core..= encryptionOption)
          ]
      )
