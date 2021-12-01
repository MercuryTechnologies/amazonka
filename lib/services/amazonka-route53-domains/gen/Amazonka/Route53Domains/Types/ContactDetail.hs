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
-- Module      : Amazonka.Route53Domains.Types.ContactDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.ContactDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Domains.Types.ContactType
import Amazonka.Route53Domains.Types.CountryCode
import Amazonka.Route53Domains.Types.ExtraParam

-- | ContactDetail includes the following elements.
--
-- /See:/ 'newContactDetail' smart constructor.
data ContactDetail = ContactDetail'
  { -- | Name of the organization for contact types other than @PERSON@.
    organizationName :: Prelude.Maybe Prelude.Text,
    -- | Email address of the contact.
    email :: Prelude.Maybe Prelude.Text,
    -- | The state or province of the contact\'s city.
    state :: Prelude.Maybe Prelude.Text,
    -- | Fax number of the contact.
    --
    -- Constraints: Phone number must be specified in the format \"+[country
    -- dialing code].[number including any area code]\". For example, a US
    -- phone number might appear as @\"+1.1234567890\"@.
    fax :: Prelude.Maybe Prelude.Text,
    -- | Last name of contact.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | A list of name-value pairs for parameters required by certain top-level
    -- domains.
    extraParams :: Prelude.Maybe [ExtraParam],
    -- | The zip or postal code of the contact\'s address.
    zipCode :: Prelude.Maybe Prelude.Text,
    -- | First line of the contact\'s address.
    addressLine1 :: Prelude.Maybe Prelude.Text,
    -- | The city of the contact\'s address.
    city :: Prelude.Maybe Prelude.Text,
    -- | The phone number of the contact.
    --
    -- Constraints: Phone number must be specified in the format \"+[country
    -- dialing code].[number including any area code>]\". For example, a US
    -- phone number might appear as @\"+1.1234567890\"@.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | Second line of contact\'s address, if any.
    addressLine2 :: Prelude.Maybe Prelude.Text,
    -- | First name of contact.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | Code for the country of the contact\'s address.
    countryCode :: Prelude.Maybe CountryCode,
    -- | Indicates whether the contact is a person, company, association, or
    -- public organization. Note the following:
    --
    -- -   If you specify a value other than @PERSON@, you must also specify a
    --     value for @OrganizationName@.
    --
    -- -   For some TLDs, the privacy protection available depends on the value
    --     that you specify for @Contact Type@. For the privacy protection
    --     settings for your TLD, see
    --     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
    --     in the /Amazon Route 53 Developer Guide/
    --
    -- -   For .es domains, if you specify @PERSON@, you must specify
    --     @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@.
    contactType :: Prelude.Maybe ContactType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationName', 'contactDetail_organizationName' - Name of the organization for contact types other than @PERSON@.
--
-- 'email', 'contactDetail_email' - Email address of the contact.
--
-- 'state', 'contactDetail_state' - The state or province of the contact\'s city.
--
-- 'fax', 'contactDetail_fax' - Fax number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
--
-- 'lastName', 'contactDetail_lastName' - Last name of contact.
--
-- 'extraParams', 'contactDetail_extraParams' - A list of name-value pairs for parameters required by certain top-level
-- domains.
--
-- 'zipCode', 'contactDetail_zipCode' - The zip or postal code of the contact\'s address.
--
-- 'addressLine1', 'contactDetail_addressLine1' - First line of the contact\'s address.
--
-- 'city', 'contactDetail_city' - The city of the contact\'s address.
--
-- 'phoneNumber', 'contactDetail_phoneNumber' - The phone number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code>]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
--
-- 'addressLine2', 'contactDetail_addressLine2' - Second line of contact\'s address, if any.
--
-- 'firstName', 'contactDetail_firstName' - First name of contact.
--
-- 'countryCode', 'contactDetail_countryCode' - Code for the country of the contact\'s address.
--
-- 'contactType', 'contactDetail_contactType' - Indicates whether the contact is a person, company, association, or
-- public organization. Note the following:
--
-- -   If you specify a value other than @PERSON@, you must also specify a
--     value for @OrganizationName@.
--
-- -   For some TLDs, the privacy protection available depends on the value
--     that you specify for @Contact Type@. For the privacy protection
--     settings for your TLD, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
--     in the /Amazon Route 53 Developer Guide/
--
-- -   For .es domains, if you specify @PERSON@, you must specify
--     @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@.
newContactDetail ::
  ContactDetail
newContactDetail =
  ContactDetail'
    { organizationName = Prelude.Nothing,
      email = Prelude.Nothing,
      state = Prelude.Nothing,
      fax = Prelude.Nothing,
      lastName = Prelude.Nothing,
      extraParams = Prelude.Nothing,
      zipCode = Prelude.Nothing,
      addressLine1 = Prelude.Nothing,
      city = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      addressLine2 = Prelude.Nothing,
      firstName = Prelude.Nothing,
      countryCode = Prelude.Nothing,
      contactType = Prelude.Nothing
    }

-- | Name of the organization for contact types other than @PERSON@.
contactDetail_organizationName :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_organizationName = Lens.lens (\ContactDetail' {organizationName} -> organizationName) (\s@ContactDetail' {} a -> s {organizationName = a} :: ContactDetail)

-- | Email address of the contact.
contactDetail_email :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_email = Lens.lens (\ContactDetail' {email} -> email) (\s@ContactDetail' {} a -> s {email = a} :: ContactDetail)

-- | The state or province of the contact\'s city.
contactDetail_state :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_state = Lens.lens (\ContactDetail' {state} -> state) (\s@ContactDetail' {} a -> s {state = a} :: ContactDetail)

-- | Fax number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
contactDetail_fax :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_fax = Lens.lens (\ContactDetail' {fax} -> fax) (\s@ContactDetail' {} a -> s {fax = a} :: ContactDetail)

-- | Last name of contact.
contactDetail_lastName :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_lastName = Lens.lens (\ContactDetail' {lastName} -> lastName) (\s@ContactDetail' {} a -> s {lastName = a} :: ContactDetail)

-- | A list of name-value pairs for parameters required by certain top-level
-- domains.
contactDetail_extraParams :: Lens.Lens' ContactDetail (Prelude.Maybe [ExtraParam])
contactDetail_extraParams = Lens.lens (\ContactDetail' {extraParams} -> extraParams) (\s@ContactDetail' {} a -> s {extraParams = a} :: ContactDetail) Prelude.. Lens.mapping Lens.coerced

-- | The zip or postal code of the contact\'s address.
contactDetail_zipCode :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_zipCode = Lens.lens (\ContactDetail' {zipCode} -> zipCode) (\s@ContactDetail' {} a -> s {zipCode = a} :: ContactDetail)

-- | First line of the contact\'s address.
contactDetail_addressLine1 :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_addressLine1 = Lens.lens (\ContactDetail' {addressLine1} -> addressLine1) (\s@ContactDetail' {} a -> s {addressLine1 = a} :: ContactDetail)

-- | The city of the contact\'s address.
contactDetail_city :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_city = Lens.lens (\ContactDetail' {city} -> city) (\s@ContactDetail' {} a -> s {city = a} :: ContactDetail)

-- | The phone number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code>]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
contactDetail_phoneNumber :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_phoneNumber = Lens.lens (\ContactDetail' {phoneNumber} -> phoneNumber) (\s@ContactDetail' {} a -> s {phoneNumber = a} :: ContactDetail)

-- | Second line of contact\'s address, if any.
contactDetail_addressLine2 :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_addressLine2 = Lens.lens (\ContactDetail' {addressLine2} -> addressLine2) (\s@ContactDetail' {} a -> s {addressLine2 = a} :: ContactDetail)

-- | First name of contact.
contactDetail_firstName :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_firstName = Lens.lens (\ContactDetail' {firstName} -> firstName) (\s@ContactDetail' {} a -> s {firstName = a} :: ContactDetail)

-- | Code for the country of the contact\'s address.
contactDetail_countryCode :: Lens.Lens' ContactDetail (Prelude.Maybe CountryCode)
contactDetail_countryCode = Lens.lens (\ContactDetail' {countryCode} -> countryCode) (\s@ContactDetail' {} a -> s {countryCode = a} :: ContactDetail)

-- | Indicates whether the contact is a person, company, association, or
-- public organization. Note the following:
--
-- -   If you specify a value other than @PERSON@, you must also specify a
--     value for @OrganizationName@.
--
-- -   For some TLDs, the privacy protection available depends on the value
--     that you specify for @Contact Type@. For the privacy protection
--     settings for your TLD, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
--     in the /Amazon Route 53 Developer Guide/
--
-- -   For .es domains, if you specify @PERSON@, you must specify
--     @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@.
contactDetail_contactType :: Lens.Lens' ContactDetail (Prelude.Maybe ContactType)
contactDetail_contactType = Lens.lens (\ContactDetail' {contactType} -> contactType) (\s@ContactDetail' {} a -> s {contactType = a} :: ContactDetail)

instance Core.FromJSON ContactDetail where
  parseJSON =
    Core.withObject
      "ContactDetail"
      ( \x ->
          ContactDetail'
            Prelude.<$> (x Core..:? "OrganizationName")
            Prelude.<*> (x Core..:? "Email")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Fax")
            Prelude.<*> (x Core..:? "LastName")
            Prelude.<*> (x Core..:? "ExtraParams" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ZipCode")
            Prelude.<*> (x Core..:? "AddressLine1")
            Prelude.<*> (x Core..:? "City")
            Prelude.<*> (x Core..:? "PhoneNumber")
            Prelude.<*> (x Core..:? "AddressLine2")
            Prelude.<*> (x Core..:? "FirstName")
            Prelude.<*> (x Core..:? "CountryCode")
            Prelude.<*> (x Core..:? "ContactType")
      )

instance Prelude.Hashable ContactDetail where
  hashWithSalt salt' ContactDetail' {..} =
    salt' `Prelude.hashWithSalt` contactType
      `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` addressLine2
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` addressLine1
      `Prelude.hashWithSalt` zipCode
      `Prelude.hashWithSalt` extraParams
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` fax
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` organizationName

instance Prelude.NFData ContactDetail where
  rnf ContactDetail' {..} =
    Prelude.rnf organizationName
      `Prelude.seq` Prelude.rnf contactType
      `Prelude.seq` Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf addressLine2
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf addressLine1
      `Prelude.seq` Prelude.rnf zipCode
      `Prelude.seq` Prelude.rnf extraParams
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf fax
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf email

instance Core.ToJSON ContactDetail where
  toJSON ContactDetail' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OrganizationName" Core..=)
              Prelude.<$> organizationName,
            ("Email" Core..=) Prelude.<$> email,
            ("State" Core..=) Prelude.<$> state,
            ("Fax" Core..=) Prelude.<$> fax,
            ("LastName" Core..=) Prelude.<$> lastName,
            ("ExtraParams" Core..=) Prelude.<$> extraParams,
            ("ZipCode" Core..=) Prelude.<$> zipCode,
            ("AddressLine1" Core..=) Prelude.<$> addressLine1,
            ("City" Core..=) Prelude.<$> city,
            ("PhoneNumber" Core..=) Prelude.<$> phoneNumber,
            ("AddressLine2" Core..=) Prelude.<$> addressLine2,
            ("FirstName" Core..=) Prelude.<$> firstName,
            ("CountryCode" Core..=) Prelude.<$> countryCode,
            ("ContactType" Core..=) Prelude.<$> contactType
          ]
      )
