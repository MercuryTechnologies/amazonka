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
-- Module      : Amazonka.VoiceId.Types.ServerSideEncryptionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.ServerSideEncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration containing information about the customer-managed KMS
-- Key used for encrypting customer data.
--
-- /See:/ 'newServerSideEncryptionConfiguration' smart constructor.
data ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration'
  { -- | The identifier of the KMS Key you want Voice ID to use to encrypt your
    -- data.
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerSideEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'serverSideEncryptionConfiguration_kmsKeyId' - The identifier of the KMS Key you want Voice ID to use to encrypt your
-- data.
newServerSideEncryptionConfiguration ::
  -- | 'kmsKeyId'
  Prelude.Text ->
  ServerSideEncryptionConfiguration
newServerSideEncryptionConfiguration pKmsKeyId_ =
  ServerSideEncryptionConfiguration'
    { kmsKeyId =
        pKmsKeyId_
    }

-- | The identifier of the KMS Key you want Voice ID to use to encrypt your
-- data.
serverSideEncryptionConfiguration_kmsKeyId :: Lens.Lens' ServerSideEncryptionConfiguration Prelude.Text
serverSideEncryptionConfiguration_kmsKeyId = Lens.lens (\ServerSideEncryptionConfiguration' {kmsKeyId} -> kmsKeyId) (\s@ServerSideEncryptionConfiguration' {} a -> s {kmsKeyId = a} :: ServerSideEncryptionConfiguration)

instance
  Core.FromJSON
    ServerSideEncryptionConfiguration
  where
  parseJSON =
    Core.withObject
      "ServerSideEncryptionConfiguration"
      ( \x ->
          ServerSideEncryptionConfiguration'
            Prelude.<$> (x Core..: "KmsKeyId")
      )

instance
  Prelude.Hashable
    ServerSideEncryptionConfiguration
  where
  hashWithSalt
    salt'
    ServerSideEncryptionConfiguration' {..} =
      salt' `Prelude.hashWithSalt` kmsKeyId

instance
  Prelude.NFData
    ServerSideEncryptionConfiguration
  where
  rnf ServerSideEncryptionConfiguration' {..} =
    Prelude.rnf kmsKeyId

instance
  Core.ToJSON
    ServerSideEncryptionConfiguration
  where
  toJSON ServerSideEncryptionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("KmsKeyId" Core..= kmsKeyId)]
      )
