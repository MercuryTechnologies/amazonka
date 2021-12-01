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
-- Module      : Amazonka.SecurityHub.Types.AwsKmsKeyDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsKmsKeyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about an KMS key.
--
-- /See:/ 'newAwsKmsKeyDetails' smart constructor.
data AwsKmsKeyDetails = AwsKmsKeyDetails'
  { -- | The source of the KMS key material.
    --
    -- When this value is @AWS_KMS@, KMS created the key material.
    --
    -- When this value is @EXTERNAL@, the key material was imported from your
    -- existing key management infrastructure or the KMS key lacks key
    -- material.
    --
    -- When this value is @AWS_CLOUDHSM@, the key material was created in the
    -- CloudHSM cluster associated with a custom key store.
    origin :: Prelude.Maybe Prelude.Text,
    -- | The manager of the KMS key. KMS keys in your Amazon Web Services account
    -- are either customer managed or Amazon Web Services managed.
    keyManager :: Prelude.Maybe Prelude.Text,
    -- | The globally unique identifier for the KMS key.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The state of the KMS key.
    keyState :: Prelude.Maybe Prelude.Text,
    -- | The twelve-digit account ID of the Amazon Web Services account that owns
    -- the KMS key.
    aWSAccountId :: Prelude.Maybe Prelude.Text,
    -- | Whether the key has key rotation enabled.
    keyRotationStatus :: Prelude.Maybe Prelude.Bool,
    -- | Indicates when the KMS key was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    creationDate :: Prelude.Maybe Prelude.Double,
    -- | A description of the key.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsKmsKeyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'origin', 'awsKmsKeyDetails_origin' - The source of the KMS key material.
--
-- When this value is @AWS_KMS@, KMS created the key material.
--
-- When this value is @EXTERNAL@, the key material was imported from your
-- existing key management infrastructure or the KMS key lacks key
-- material.
--
-- When this value is @AWS_CLOUDHSM@, the key material was created in the
-- CloudHSM cluster associated with a custom key store.
--
-- 'keyManager', 'awsKmsKeyDetails_keyManager' - The manager of the KMS key. KMS keys in your Amazon Web Services account
-- are either customer managed or Amazon Web Services managed.
--
-- 'keyId', 'awsKmsKeyDetails_keyId' - The globally unique identifier for the KMS key.
--
-- 'keyState', 'awsKmsKeyDetails_keyState' - The state of the KMS key.
--
-- 'aWSAccountId', 'awsKmsKeyDetails_aWSAccountId' - The twelve-digit account ID of the Amazon Web Services account that owns
-- the KMS key.
--
-- 'keyRotationStatus', 'awsKmsKeyDetails_keyRotationStatus' - Whether the key has key rotation enabled.
--
-- 'creationDate', 'awsKmsKeyDetails_creationDate' - Indicates when the KMS key was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'description', 'awsKmsKeyDetails_description' - A description of the key.
newAwsKmsKeyDetails ::
  AwsKmsKeyDetails
newAwsKmsKeyDetails =
  AwsKmsKeyDetails'
    { origin = Prelude.Nothing,
      keyManager = Prelude.Nothing,
      keyId = Prelude.Nothing,
      keyState = Prelude.Nothing,
      aWSAccountId = Prelude.Nothing,
      keyRotationStatus = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The source of the KMS key material.
--
-- When this value is @AWS_KMS@, KMS created the key material.
--
-- When this value is @EXTERNAL@, the key material was imported from your
-- existing key management infrastructure or the KMS key lacks key
-- material.
--
-- When this value is @AWS_CLOUDHSM@, the key material was created in the
-- CloudHSM cluster associated with a custom key store.
awsKmsKeyDetails_origin :: Lens.Lens' AwsKmsKeyDetails (Prelude.Maybe Prelude.Text)
awsKmsKeyDetails_origin = Lens.lens (\AwsKmsKeyDetails' {origin} -> origin) (\s@AwsKmsKeyDetails' {} a -> s {origin = a} :: AwsKmsKeyDetails)

-- | The manager of the KMS key. KMS keys in your Amazon Web Services account
-- are either customer managed or Amazon Web Services managed.
awsKmsKeyDetails_keyManager :: Lens.Lens' AwsKmsKeyDetails (Prelude.Maybe Prelude.Text)
awsKmsKeyDetails_keyManager = Lens.lens (\AwsKmsKeyDetails' {keyManager} -> keyManager) (\s@AwsKmsKeyDetails' {} a -> s {keyManager = a} :: AwsKmsKeyDetails)

-- | The globally unique identifier for the KMS key.
awsKmsKeyDetails_keyId :: Lens.Lens' AwsKmsKeyDetails (Prelude.Maybe Prelude.Text)
awsKmsKeyDetails_keyId = Lens.lens (\AwsKmsKeyDetails' {keyId} -> keyId) (\s@AwsKmsKeyDetails' {} a -> s {keyId = a} :: AwsKmsKeyDetails)

-- | The state of the KMS key.
awsKmsKeyDetails_keyState :: Lens.Lens' AwsKmsKeyDetails (Prelude.Maybe Prelude.Text)
awsKmsKeyDetails_keyState = Lens.lens (\AwsKmsKeyDetails' {keyState} -> keyState) (\s@AwsKmsKeyDetails' {} a -> s {keyState = a} :: AwsKmsKeyDetails)

-- | The twelve-digit account ID of the Amazon Web Services account that owns
-- the KMS key.
awsKmsKeyDetails_aWSAccountId :: Lens.Lens' AwsKmsKeyDetails (Prelude.Maybe Prelude.Text)
awsKmsKeyDetails_aWSAccountId = Lens.lens (\AwsKmsKeyDetails' {aWSAccountId} -> aWSAccountId) (\s@AwsKmsKeyDetails' {} a -> s {aWSAccountId = a} :: AwsKmsKeyDetails)

-- | Whether the key has key rotation enabled.
awsKmsKeyDetails_keyRotationStatus :: Lens.Lens' AwsKmsKeyDetails (Prelude.Maybe Prelude.Bool)
awsKmsKeyDetails_keyRotationStatus = Lens.lens (\AwsKmsKeyDetails' {keyRotationStatus} -> keyRotationStatus) (\s@AwsKmsKeyDetails' {} a -> s {keyRotationStatus = a} :: AwsKmsKeyDetails)

-- | Indicates when the KMS key was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsKmsKeyDetails_creationDate :: Lens.Lens' AwsKmsKeyDetails (Prelude.Maybe Prelude.Double)
awsKmsKeyDetails_creationDate = Lens.lens (\AwsKmsKeyDetails' {creationDate} -> creationDate) (\s@AwsKmsKeyDetails' {} a -> s {creationDate = a} :: AwsKmsKeyDetails)

-- | A description of the key.
awsKmsKeyDetails_description :: Lens.Lens' AwsKmsKeyDetails (Prelude.Maybe Prelude.Text)
awsKmsKeyDetails_description = Lens.lens (\AwsKmsKeyDetails' {description} -> description) (\s@AwsKmsKeyDetails' {} a -> s {description = a} :: AwsKmsKeyDetails)

instance Core.FromJSON AwsKmsKeyDetails where
  parseJSON =
    Core.withObject
      "AwsKmsKeyDetails"
      ( \x ->
          AwsKmsKeyDetails'
            Prelude.<$> (x Core..:? "Origin")
            Prelude.<*> (x Core..:? "KeyManager")
            Prelude.<*> (x Core..:? "KeyId")
            Prelude.<*> (x Core..:? "KeyState")
            Prelude.<*> (x Core..:? "AWSAccountId")
            Prelude.<*> (x Core..:? "KeyRotationStatus")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable AwsKmsKeyDetails where
  hashWithSalt salt' AwsKmsKeyDetails' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` keyRotationStatus
      `Prelude.hashWithSalt` aWSAccountId
      `Prelude.hashWithSalt` keyState
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` keyManager
      `Prelude.hashWithSalt` origin

instance Prelude.NFData AwsKmsKeyDetails where
  rnf AwsKmsKeyDetails' {..} =
    Prelude.rnf origin
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf keyRotationStatus
      `Prelude.seq` Prelude.rnf aWSAccountId
      `Prelude.seq` Prelude.rnf keyState
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf keyManager

instance Core.ToJSON AwsKmsKeyDetails where
  toJSON AwsKmsKeyDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Origin" Core..=) Prelude.<$> origin,
            ("KeyManager" Core..=) Prelude.<$> keyManager,
            ("KeyId" Core..=) Prelude.<$> keyId,
            ("KeyState" Core..=) Prelude.<$> keyState,
            ("AWSAccountId" Core..=) Prelude.<$> aWSAccountId,
            ("KeyRotationStatus" Core..=)
              Prelude.<$> keyRotationStatus,
            ("CreationDate" Core..=) Prelude.<$> creationDate,
            ("Description" Core..=) Prelude.<$> description
          ]
      )
