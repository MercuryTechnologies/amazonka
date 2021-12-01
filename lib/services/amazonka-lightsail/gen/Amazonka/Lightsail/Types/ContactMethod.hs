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
-- Module      : Amazonka.Lightsail.Types.ContactMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContactMethod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.ContactMethodStatus
import Amazonka.Lightsail.Types.ContactProtocol
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes a contact method.
--
-- A contact method is a way to send you notifications. For more
-- information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail>.
--
-- /See:/ 'newContactMethod' smart constructor.
data ContactMethod = ContactMethod'
  { -- | The current status of the contact method.
    --
    -- A contact method has the following possible status:
    --
    -- -   @PendingVerification@ - The contact method has not yet been
    --     verified, and the verification has not yet expired.
    --
    -- -   @Valid@ - The contact method has been verified.
    --
    -- -   @InValid@ - An attempt was made to verify the contact method, but
    --     the verification has expired.
    status :: Prelude.Maybe ContactMethodStatus,
    -- | The Lightsail resource type (e.g., @ContactMethod@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the contact method.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the contact method was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    location :: Prelude.Maybe ResourceLocation,
    -- | The protocol of the contact method, such as email or SMS (text
    -- messaging).
    protocol :: Prelude.Maybe ContactProtocol,
    -- | The name of the contact method.
    name :: Prelude.Maybe Prelude.Text,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail contact method. This code enables
    -- our support team to look up your Lightsail information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The destination of the contact method, such as an email address or a
    -- mobile phone number.
    contactEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'contactMethod_status' - The current status of the contact method.
--
-- A contact method has the following possible status:
--
-- -   @PendingVerification@ - The contact method has not yet been
--     verified, and the verification has not yet expired.
--
-- -   @Valid@ - The contact method has been verified.
--
-- -   @InValid@ - An attempt was made to verify the contact method, but
--     the verification has expired.
--
-- 'resourceType', 'contactMethod_resourceType' - The Lightsail resource type (e.g., @ContactMethod@).
--
-- 'arn', 'contactMethod_arn' - The Amazon Resource Name (ARN) of the contact method.
--
-- 'createdAt', 'contactMethod_createdAt' - The timestamp when the contact method was created.
--
-- 'location', 'contactMethod_location' - Undocumented member.
--
-- 'protocol', 'contactMethod_protocol' - The protocol of the contact method, such as email or SMS (text
-- messaging).
--
-- 'name', 'contactMethod_name' - The name of the contact method.
--
-- 'supportCode', 'contactMethod_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail contact method. This code enables
-- our support team to look up your Lightsail information more easily.
--
-- 'contactEndpoint', 'contactMethod_contactEndpoint' - The destination of the contact method, such as an email address or a
-- mobile phone number.
newContactMethod ::
  ContactMethod
newContactMethod =
  ContactMethod'
    { status = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      location = Prelude.Nothing,
      protocol = Prelude.Nothing,
      name = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      contactEndpoint = Prelude.Nothing
    }

-- | The current status of the contact method.
--
-- A contact method has the following possible status:
--
-- -   @PendingVerification@ - The contact method has not yet been
--     verified, and the verification has not yet expired.
--
-- -   @Valid@ - The contact method has been verified.
--
-- -   @InValid@ - An attempt was made to verify the contact method, but
--     the verification has expired.
contactMethod_status :: Lens.Lens' ContactMethod (Prelude.Maybe ContactMethodStatus)
contactMethod_status = Lens.lens (\ContactMethod' {status} -> status) (\s@ContactMethod' {} a -> s {status = a} :: ContactMethod)

-- | The Lightsail resource type (e.g., @ContactMethod@).
contactMethod_resourceType :: Lens.Lens' ContactMethod (Prelude.Maybe ResourceType)
contactMethod_resourceType = Lens.lens (\ContactMethod' {resourceType} -> resourceType) (\s@ContactMethod' {} a -> s {resourceType = a} :: ContactMethod)

-- | The Amazon Resource Name (ARN) of the contact method.
contactMethod_arn :: Lens.Lens' ContactMethod (Prelude.Maybe Prelude.Text)
contactMethod_arn = Lens.lens (\ContactMethod' {arn} -> arn) (\s@ContactMethod' {} a -> s {arn = a} :: ContactMethod)

-- | The timestamp when the contact method was created.
contactMethod_createdAt :: Lens.Lens' ContactMethod (Prelude.Maybe Prelude.UTCTime)
contactMethod_createdAt = Lens.lens (\ContactMethod' {createdAt} -> createdAt) (\s@ContactMethod' {} a -> s {createdAt = a} :: ContactMethod) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
contactMethod_location :: Lens.Lens' ContactMethod (Prelude.Maybe ResourceLocation)
contactMethod_location = Lens.lens (\ContactMethod' {location} -> location) (\s@ContactMethod' {} a -> s {location = a} :: ContactMethod)

-- | The protocol of the contact method, such as email or SMS (text
-- messaging).
contactMethod_protocol :: Lens.Lens' ContactMethod (Prelude.Maybe ContactProtocol)
contactMethod_protocol = Lens.lens (\ContactMethod' {protocol} -> protocol) (\s@ContactMethod' {} a -> s {protocol = a} :: ContactMethod)

-- | The name of the contact method.
contactMethod_name :: Lens.Lens' ContactMethod (Prelude.Maybe Prelude.Text)
contactMethod_name = Lens.lens (\ContactMethod' {name} -> name) (\s@ContactMethod' {} a -> s {name = a} :: ContactMethod)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail contact method. This code enables
-- our support team to look up your Lightsail information more easily.
contactMethod_supportCode :: Lens.Lens' ContactMethod (Prelude.Maybe Prelude.Text)
contactMethod_supportCode = Lens.lens (\ContactMethod' {supportCode} -> supportCode) (\s@ContactMethod' {} a -> s {supportCode = a} :: ContactMethod)

-- | The destination of the contact method, such as an email address or a
-- mobile phone number.
contactMethod_contactEndpoint :: Lens.Lens' ContactMethod (Prelude.Maybe Prelude.Text)
contactMethod_contactEndpoint = Lens.lens (\ContactMethod' {contactEndpoint} -> contactEndpoint) (\s@ContactMethod' {} a -> s {contactEndpoint = a} :: ContactMethod)

instance Core.FromJSON ContactMethod where
  parseJSON =
    Core.withObject
      "ContactMethod"
      ( \x ->
          ContactMethod'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "protocol")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "contactEndpoint")
      )

instance Prelude.Hashable ContactMethod where
  hashWithSalt salt' ContactMethod' {..} =
    salt' `Prelude.hashWithSalt` contactEndpoint
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` status

instance Prelude.NFData ContactMethod where
  rnf ContactMethod' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf contactEndpoint
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf resourceType
