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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UserPoolType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UserPoolType where

import Amazonka.CognitoIdentityProvider.Types.AccountRecoverySettingType
import Amazonka.CognitoIdentityProvider.Types.AdminCreateUserConfigType
import Amazonka.CognitoIdentityProvider.Types.AliasAttributeType
import Amazonka.CognitoIdentityProvider.Types.DeviceConfigurationType
import Amazonka.CognitoIdentityProvider.Types.EmailConfigurationType
import Amazonka.CognitoIdentityProvider.Types.LambdaConfigType
import Amazonka.CognitoIdentityProvider.Types.SchemaAttributeType
import Amazonka.CognitoIdentityProvider.Types.SmsConfigurationType
import Amazonka.CognitoIdentityProvider.Types.StatusType
import Amazonka.CognitoIdentityProvider.Types.UserPoolAddOnsType
import Amazonka.CognitoIdentityProvider.Types.UserPoolMfaType
import Amazonka.CognitoIdentityProvider.Types.UserPoolPolicyType
import Amazonka.CognitoIdentityProvider.Types.UsernameAttributeType
import Amazonka.CognitoIdentityProvider.Types.UsernameConfigurationType
import Amazonka.CognitoIdentityProvider.Types.VerificationMessageTemplateType
import Amazonka.CognitoIdentityProvider.Types.VerifiedAttributeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A container for information about the user pool.
--
-- /See:/ 'newUserPoolType' smart constructor.
data UserPoolType = UserPoolType'
  { -- | The status of a user pool.
    status :: Prelude.Maybe StatusType,
    -- | The tags that are assigned to the user pool. A tag is a label that you
    -- can apply to user pools to categorize and manage them in different ways,
    -- such as by purpose, owner, environment, or other criteria.
    userPoolTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The reason why the email configuration cannot send the messages to your
    -- users.
    emailConfigurationFailure :: Prelude.Maybe Prelude.Text,
    -- | The date the user pool was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The template for verification messages.
    verificationMessageTemplate :: Prelude.Maybe VerificationMessageTemplateType,
    -- | A number estimating the size of the user pool.
    estimatedNumberOfUsers :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) for the user pool.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Holds the domain prefix if the user pool has a domain associated with
    -- it.
    domain :: Prelude.Maybe Prelude.Text,
    -- | A custom domain name that you provide to Amazon Cognito. This parameter
    -- applies only if you use a custom domain to host the sign-up and sign-in
    -- pages for your application. For example: @auth.example.com@.
    --
    -- For more information about adding a custom domain to your user pool, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI>.
    customDomain :: Prelude.Maybe Prelude.Text,
    -- | The contents of the email verification message.
    emailVerificationMessage :: Prelude.Maybe Prelude.Text,
    -- | The contents of the SMS authentication message.
    smsAuthenticationMessage :: Prelude.Maybe Prelude.Text,
    -- | The user pool add-ons.
    userPoolAddOns :: Prelude.Maybe UserPoolAddOnsType,
    -- | A container with the schema attributes of a user pool.
    schemaAttributes :: Prelude.Maybe (Prelude.NonEmpty SchemaAttributeType),
    -- | The subject of the email verification message.
    emailVerificationSubject :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether email addresses or phone numbers can be specified as
    -- usernames when a user signs up.
    usernameAttributes :: Prelude.Maybe [UsernameAttributeType],
    -- | Specifies the attributes that are aliased in a user pool.
    aliasAttributes :: Prelude.Maybe [AliasAttributeType],
    -- | Use this setting to define which verified available method a user can
    -- use to recover their password when they call @ForgotPassword@. It allows
    -- you to define a preferred method when a user has more than one method
    -- available. With this setting, SMS does not qualify for a valid password
    -- recovery mechanism if the user also has SMS MFA enabled. In the absence
    -- of this setting, Cognito uses the legacy behavior to determine the
    -- recovery method where SMS is preferred over email.
    accountRecoverySetting :: Prelude.Maybe AccountRecoverySettingType,
    -- | The email configuration.
    emailConfiguration :: Prelude.Maybe EmailConfigurationType,
    -- | The contents of the SMS verification message.
    smsVerificationMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the user pool.
    name :: Prelude.Maybe Prelude.Text,
    -- | Can be one of the following values:
    --
    -- -   @OFF@ - MFA tokens are not required and cannot be specified during
    --     user registration.
    --
    -- -   @ON@ - MFA tokens are required for all user registrations. You can
    --     only specify required when you are initially creating a user pool.
    --
    -- -   @OPTIONAL@ - Users have the option when registering to create an MFA
    --     token.
    mfaConfiguration :: Prelude.Maybe UserPoolMfaType,
    -- | The ID of the user pool.
    id :: Prelude.Maybe Prelude.Text,
    -- | The reason why the SMS configuration cannot send the messages to your
    -- users.
    --
    -- This message might include comma-separated values to describe why your
    -- SMS configuration can\'t send messages to user pool end users.
    --
    -- -   InvalidSmsRoleAccessPolicyException - The IAM role which Cognito
    --     uses to send SMS messages is not properly configured. For more
    --     information, see
    --     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_SmsConfigurationType.html SmsConfigurationType>.
    --
    -- -   SNSSandbox - The account is in SNS Sandbox and messages won’t reach
    --     unverified end users. This parameter won’t get populated with
    --     SNSSandbox if the IAM user creating the user pool doesn’t have SNS
    --     permissions. To learn how to move your account out of the sandbox,
    --     see
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox-moving-to-production.html Moving out of the SMS sandbox>.
    smsConfigurationFailure :: Prelude.Maybe Prelude.Text,
    -- | The date the user pool was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The Lambda triggers associated with the user pool.
    lambdaConfig :: Prelude.Maybe LambdaConfigType,
    -- | The SMS configuration.
    smsConfiguration :: Prelude.Maybe SmsConfigurationType,
    -- | The configuration for @AdminCreateUser@ requests.
    adminCreateUserConfig :: Prelude.Maybe AdminCreateUserConfigType,
    -- | The device configuration.
    deviceConfiguration :: Prelude.Maybe DeviceConfigurationType,
    -- | Specifies the attributes that are auto-verified in a user pool.
    autoVerifiedAttributes :: Prelude.Maybe [VerifiedAttributeType],
    -- | The policies associated with the user pool.
    policies :: Prelude.Maybe UserPoolPolicyType,
    -- | You can choose to enable case sensitivity on the username input for the
    -- selected sign-in option. For example, when this is set to @False@, users
    -- will be able to sign in using either \"username\" or \"Username\". This
    -- configuration is immutable once it has been set. For more information,
    -- see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
    usernameConfiguration :: Prelude.Maybe UsernameConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPoolType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'userPoolType_status' - The status of a user pool.
--
-- 'userPoolTags', 'userPoolType_userPoolTags' - The tags that are assigned to the user pool. A tag is a label that you
-- can apply to user pools to categorize and manage them in different ways,
-- such as by purpose, owner, environment, or other criteria.
--
-- 'emailConfigurationFailure', 'userPoolType_emailConfigurationFailure' - The reason why the email configuration cannot send the messages to your
-- users.
--
-- 'lastModifiedDate', 'userPoolType_lastModifiedDate' - The date the user pool was last modified.
--
-- 'verificationMessageTemplate', 'userPoolType_verificationMessageTemplate' - The template for verification messages.
--
-- 'estimatedNumberOfUsers', 'userPoolType_estimatedNumberOfUsers' - A number estimating the size of the user pool.
--
-- 'arn', 'userPoolType_arn' - The Amazon Resource Name (ARN) for the user pool.
--
-- 'domain', 'userPoolType_domain' - Holds the domain prefix if the user pool has a domain associated with
-- it.
--
-- 'customDomain', 'userPoolType_customDomain' - A custom domain name that you provide to Amazon Cognito. This parameter
-- applies only if you use a custom domain to host the sign-up and sign-in
-- pages for your application. For example: @auth.example.com@.
--
-- For more information about adding a custom domain to your user pool, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI>.
--
-- 'emailVerificationMessage', 'userPoolType_emailVerificationMessage' - The contents of the email verification message.
--
-- 'smsAuthenticationMessage', 'userPoolType_smsAuthenticationMessage' - The contents of the SMS authentication message.
--
-- 'userPoolAddOns', 'userPoolType_userPoolAddOns' - The user pool add-ons.
--
-- 'schemaAttributes', 'userPoolType_schemaAttributes' - A container with the schema attributes of a user pool.
--
-- 'emailVerificationSubject', 'userPoolType_emailVerificationSubject' - The subject of the email verification message.
--
-- 'usernameAttributes', 'userPoolType_usernameAttributes' - Specifies whether email addresses or phone numbers can be specified as
-- usernames when a user signs up.
--
-- 'aliasAttributes', 'userPoolType_aliasAttributes' - Specifies the attributes that are aliased in a user pool.
--
-- 'accountRecoverySetting', 'userPoolType_accountRecoverySetting' - Use this setting to define which verified available method a user can
-- use to recover their password when they call @ForgotPassword@. It allows
-- you to define a preferred method when a user has more than one method
-- available. With this setting, SMS does not qualify for a valid password
-- recovery mechanism if the user also has SMS MFA enabled. In the absence
-- of this setting, Cognito uses the legacy behavior to determine the
-- recovery method where SMS is preferred over email.
--
-- 'emailConfiguration', 'userPoolType_emailConfiguration' - The email configuration.
--
-- 'smsVerificationMessage', 'userPoolType_smsVerificationMessage' - The contents of the SMS verification message.
--
-- 'name', 'userPoolType_name' - The name of the user pool.
--
-- 'mfaConfiguration', 'userPoolType_mfaConfiguration' - Can be one of the following values:
--
-- -   @OFF@ - MFA tokens are not required and cannot be specified during
--     user registration.
--
-- -   @ON@ - MFA tokens are required for all user registrations. You can
--     only specify required when you are initially creating a user pool.
--
-- -   @OPTIONAL@ - Users have the option when registering to create an MFA
--     token.
--
-- 'id', 'userPoolType_id' - The ID of the user pool.
--
-- 'smsConfigurationFailure', 'userPoolType_smsConfigurationFailure' - The reason why the SMS configuration cannot send the messages to your
-- users.
--
-- This message might include comma-separated values to describe why your
-- SMS configuration can\'t send messages to user pool end users.
--
-- -   InvalidSmsRoleAccessPolicyException - The IAM role which Cognito
--     uses to send SMS messages is not properly configured. For more
--     information, see
--     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_SmsConfigurationType.html SmsConfigurationType>.
--
-- -   SNSSandbox - The account is in SNS Sandbox and messages won’t reach
--     unverified end users. This parameter won’t get populated with
--     SNSSandbox if the IAM user creating the user pool doesn’t have SNS
--     permissions. To learn how to move your account out of the sandbox,
--     see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox-moving-to-production.html Moving out of the SMS sandbox>.
--
-- 'creationDate', 'userPoolType_creationDate' - The date the user pool was created.
--
-- 'lambdaConfig', 'userPoolType_lambdaConfig' - The Lambda triggers associated with the user pool.
--
-- 'smsConfiguration', 'userPoolType_smsConfiguration' - The SMS configuration.
--
-- 'adminCreateUserConfig', 'userPoolType_adminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
--
-- 'deviceConfiguration', 'userPoolType_deviceConfiguration' - The device configuration.
--
-- 'autoVerifiedAttributes', 'userPoolType_autoVerifiedAttributes' - Specifies the attributes that are auto-verified in a user pool.
--
-- 'policies', 'userPoolType_policies' - The policies associated with the user pool.
--
-- 'usernameConfiguration', 'userPoolType_usernameConfiguration' - You can choose to enable case sensitivity on the username input for the
-- selected sign-in option. For example, when this is set to @False@, users
-- will be able to sign in using either \"username\" or \"Username\". This
-- configuration is immutable once it has been set. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
newUserPoolType ::
  UserPoolType
newUserPoolType =
  UserPoolType'
    { status = Prelude.Nothing,
      userPoolTags = Prelude.Nothing,
      emailConfigurationFailure = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      verificationMessageTemplate = Prelude.Nothing,
      estimatedNumberOfUsers = Prelude.Nothing,
      arn = Prelude.Nothing,
      domain = Prelude.Nothing,
      customDomain = Prelude.Nothing,
      emailVerificationMessage = Prelude.Nothing,
      smsAuthenticationMessage = Prelude.Nothing,
      userPoolAddOns = Prelude.Nothing,
      schemaAttributes = Prelude.Nothing,
      emailVerificationSubject = Prelude.Nothing,
      usernameAttributes = Prelude.Nothing,
      aliasAttributes = Prelude.Nothing,
      accountRecoverySetting = Prelude.Nothing,
      emailConfiguration = Prelude.Nothing,
      smsVerificationMessage = Prelude.Nothing,
      name = Prelude.Nothing,
      mfaConfiguration = Prelude.Nothing,
      id = Prelude.Nothing,
      smsConfigurationFailure = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      smsConfiguration = Prelude.Nothing,
      adminCreateUserConfig = Prelude.Nothing,
      deviceConfiguration = Prelude.Nothing,
      autoVerifiedAttributes = Prelude.Nothing,
      policies = Prelude.Nothing,
      usernameConfiguration = Prelude.Nothing
    }

-- | The status of a user pool.
userPoolType_status :: Lens.Lens' UserPoolType (Prelude.Maybe StatusType)
userPoolType_status = Lens.lens (\UserPoolType' {status} -> status) (\s@UserPoolType' {} a -> s {status = a} :: UserPoolType)

-- | The tags that are assigned to the user pool. A tag is a label that you
-- can apply to user pools to categorize and manage them in different ways,
-- such as by purpose, owner, environment, or other criteria.
userPoolType_userPoolTags :: Lens.Lens' UserPoolType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
userPoolType_userPoolTags = Lens.lens (\UserPoolType' {userPoolTags} -> userPoolTags) (\s@UserPoolType' {} a -> s {userPoolTags = a} :: UserPoolType) Prelude.. Lens.mapping Lens.coerced

-- | The reason why the email configuration cannot send the messages to your
-- users.
userPoolType_emailConfigurationFailure :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_emailConfigurationFailure = Lens.lens (\UserPoolType' {emailConfigurationFailure} -> emailConfigurationFailure) (\s@UserPoolType' {} a -> s {emailConfigurationFailure = a} :: UserPoolType)

-- | The date the user pool was last modified.
userPoolType_lastModifiedDate :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.UTCTime)
userPoolType_lastModifiedDate = Lens.lens (\UserPoolType' {lastModifiedDate} -> lastModifiedDate) (\s@UserPoolType' {} a -> s {lastModifiedDate = a} :: UserPoolType) Prelude.. Lens.mapping Core._Time

-- | The template for verification messages.
userPoolType_verificationMessageTemplate :: Lens.Lens' UserPoolType (Prelude.Maybe VerificationMessageTemplateType)
userPoolType_verificationMessageTemplate = Lens.lens (\UserPoolType' {verificationMessageTemplate} -> verificationMessageTemplate) (\s@UserPoolType' {} a -> s {verificationMessageTemplate = a} :: UserPoolType)

-- | A number estimating the size of the user pool.
userPoolType_estimatedNumberOfUsers :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Int)
userPoolType_estimatedNumberOfUsers = Lens.lens (\UserPoolType' {estimatedNumberOfUsers} -> estimatedNumberOfUsers) (\s@UserPoolType' {} a -> s {estimatedNumberOfUsers = a} :: UserPoolType)

-- | The Amazon Resource Name (ARN) for the user pool.
userPoolType_arn :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_arn = Lens.lens (\UserPoolType' {arn} -> arn) (\s@UserPoolType' {} a -> s {arn = a} :: UserPoolType)

-- | Holds the domain prefix if the user pool has a domain associated with
-- it.
userPoolType_domain :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_domain = Lens.lens (\UserPoolType' {domain} -> domain) (\s@UserPoolType' {} a -> s {domain = a} :: UserPoolType)

-- | A custom domain name that you provide to Amazon Cognito. This parameter
-- applies only if you use a custom domain to host the sign-up and sign-in
-- pages for your application. For example: @auth.example.com@.
--
-- For more information about adding a custom domain to your user pool, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI>.
userPoolType_customDomain :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_customDomain = Lens.lens (\UserPoolType' {customDomain} -> customDomain) (\s@UserPoolType' {} a -> s {customDomain = a} :: UserPoolType)

-- | The contents of the email verification message.
userPoolType_emailVerificationMessage :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_emailVerificationMessage = Lens.lens (\UserPoolType' {emailVerificationMessage} -> emailVerificationMessage) (\s@UserPoolType' {} a -> s {emailVerificationMessage = a} :: UserPoolType)

-- | The contents of the SMS authentication message.
userPoolType_smsAuthenticationMessage :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_smsAuthenticationMessage = Lens.lens (\UserPoolType' {smsAuthenticationMessage} -> smsAuthenticationMessage) (\s@UserPoolType' {} a -> s {smsAuthenticationMessage = a} :: UserPoolType)

-- | The user pool add-ons.
userPoolType_userPoolAddOns :: Lens.Lens' UserPoolType (Prelude.Maybe UserPoolAddOnsType)
userPoolType_userPoolAddOns = Lens.lens (\UserPoolType' {userPoolAddOns} -> userPoolAddOns) (\s@UserPoolType' {} a -> s {userPoolAddOns = a} :: UserPoolType)

-- | A container with the schema attributes of a user pool.
userPoolType_schemaAttributes :: Lens.Lens' UserPoolType (Prelude.Maybe (Prelude.NonEmpty SchemaAttributeType))
userPoolType_schemaAttributes = Lens.lens (\UserPoolType' {schemaAttributes} -> schemaAttributes) (\s@UserPoolType' {} a -> s {schemaAttributes = a} :: UserPoolType) Prelude.. Lens.mapping Lens.coerced

-- | The subject of the email verification message.
userPoolType_emailVerificationSubject :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_emailVerificationSubject = Lens.lens (\UserPoolType' {emailVerificationSubject} -> emailVerificationSubject) (\s@UserPoolType' {} a -> s {emailVerificationSubject = a} :: UserPoolType)

-- | Specifies whether email addresses or phone numbers can be specified as
-- usernames when a user signs up.
userPoolType_usernameAttributes :: Lens.Lens' UserPoolType (Prelude.Maybe [UsernameAttributeType])
userPoolType_usernameAttributes = Lens.lens (\UserPoolType' {usernameAttributes} -> usernameAttributes) (\s@UserPoolType' {} a -> s {usernameAttributes = a} :: UserPoolType) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the attributes that are aliased in a user pool.
userPoolType_aliasAttributes :: Lens.Lens' UserPoolType (Prelude.Maybe [AliasAttributeType])
userPoolType_aliasAttributes = Lens.lens (\UserPoolType' {aliasAttributes} -> aliasAttributes) (\s@UserPoolType' {} a -> s {aliasAttributes = a} :: UserPoolType) Prelude.. Lens.mapping Lens.coerced

-- | Use this setting to define which verified available method a user can
-- use to recover their password when they call @ForgotPassword@. It allows
-- you to define a preferred method when a user has more than one method
-- available. With this setting, SMS does not qualify for a valid password
-- recovery mechanism if the user also has SMS MFA enabled. In the absence
-- of this setting, Cognito uses the legacy behavior to determine the
-- recovery method where SMS is preferred over email.
userPoolType_accountRecoverySetting :: Lens.Lens' UserPoolType (Prelude.Maybe AccountRecoverySettingType)
userPoolType_accountRecoverySetting = Lens.lens (\UserPoolType' {accountRecoverySetting} -> accountRecoverySetting) (\s@UserPoolType' {} a -> s {accountRecoverySetting = a} :: UserPoolType)

-- | The email configuration.
userPoolType_emailConfiguration :: Lens.Lens' UserPoolType (Prelude.Maybe EmailConfigurationType)
userPoolType_emailConfiguration = Lens.lens (\UserPoolType' {emailConfiguration} -> emailConfiguration) (\s@UserPoolType' {} a -> s {emailConfiguration = a} :: UserPoolType)

-- | The contents of the SMS verification message.
userPoolType_smsVerificationMessage :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_smsVerificationMessage = Lens.lens (\UserPoolType' {smsVerificationMessage} -> smsVerificationMessage) (\s@UserPoolType' {} a -> s {smsVerificationMessage = a} :: UserPoolType)

-- | The name of the user pool.
userPoolType_name :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_name = Lens.lens (\UserPoolType' {name} -> name) (\s@UserPoolType' {} a -> s {name = a} :: UserPoolType)

-- | Can be one of the following values:
--
-- -   @OFF@ - MFA tokens are not required and cannot be specified during
--     user registration.
--
-- -   @ON@ - MFA tokens are required for all user registrations. You can
--     only specify required when you are initially creating a user pool.
--
-- -   @OPTIONAL@ - Users have the option when registering to create an MFA
--     token.
userPoolType_mfaConfiguration :: Lens.Lens' UserPoolType (Prelude.Maybe UserPoolMfaType)
userPoolType_mfaConfiguration = Lens.lens (\UserPoolType' {mfaConfiguration} -> mfaConfiguration) (\s@UserPoolType' {} a -> s {mfaConfiguration = a} :: UserPoolType)

-- | The ID of the user pool.
userPoolType_id :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_id = Lens.lens (\UserPoolType' {id} -> id) (\s@UserPoolType' {} a -> s {id = a} :: UserPoolType)

-- | The reason why the SMS configuration cannot send the messages to your
-- users.
--
-- This message might include comma-separated values to describe why your
-- SMS configuration can\'t send messages to user pool end users.
--
-- -   InvalidSmsRoleAccessPolicyException - The IAM role which Cognito
--     uses to send SMS messages is not properly configured. For more
--     information, see
--     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_SmsConfigurationType.html SmsConfigurationType>.
--
-- -   SNSSandbox - The account is in SNS Sandbox and messages won’t reach
--     unverified end users. This parameter won’t get populated with
--     SNSSandbox if the IAM user creating the user pool doesn’t have SNS
--     permissions. To learn how to move your account out of the sandbox,
--     see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox-moving-to-production.html Moving out of the SMS sandbox>.
userPoolType_smsConfigurationFailure :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_smsConfigurationFailure = Lens.lens (\UserPoolType' {smsConfigurationFailure} -> smsConfigurationFailure) (\s@UserPoolType' {} a -> s {smsConfigurationFailure = a} :: UserPoolType)

-- | The date the user pool was created.
userPoolType_creationDate :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.UTCTime)
userPoolType_creationDate = Lens.lens (\UserPoolType' {creationDate} -> creationDate) (\s@UserPoolType' {} a -> s {creationDate = a} :: UserPoolType) Prelude.. Lens.mapping Core._Time

-- | The Lambda triggers associated with the user pool.
userPoolType_lambdaConfig :: Lens.Lens' UserPoolType (Prelude.Maybe LambdaConfigType)
userPoolType_lambdaConfig = Lens.lens (\UserPoolType' {lambdaConfig} -> lambdaConfig) (\s@UserPoolType' {} a -> s {lambdaConfig = a} :: UserPoolType)

-- | The SMS configuration.
userPoolType_smsConfiguration :: Lens.Lens' UserPoolType (Prelude.Maybe SmsConfigurationType)
userPoolType_smsConfiguration = Lens.lens (\UserPoolType' {smsConfiguration} -> smsConfiguration) (\s@UserPoolType' {} a -> s {smsConfiguration = a} :: UserPoolType)

-- | The configuration for @AdminCreateUser@ requests.
userPoolType_adminCreateUserConfig :: Lens.Lens' UserPoolType (Prelude.Maybe AdminCreateUserConfigType)
userPoolType_adminCreateUserConfig = Lens.lens (\UserPoolType' {adminCreateUserConfig} -> adminCreateUserConfig) (\s@UserPoolType' {} a -> s {adminCreateUserConfig = a} :: UserPoolType)

-- | The device configuration.
userPoolType_deviceConfiguration :: Lens.Lens' UserPoolType (Prelude.Maybe DeviceConfigurationType)
userPoolType_deviceConfiguration = Lens.lens (\UserPoolType' {deviceConfiguration} -> deviceConfiguration) (\s@UserPoolType' {} a -> s {deviceConfiguration = a} :: UserPoolType)

-- | Specifies the attributes that are auto-verified in a user pool.
userPoolType_autoVerifiedAttributes :: Lens.Lens' UserPoolType (Prelude.Maybe [VerifiedAttributeType])
userPoolType_autoVerifiedAttributes = Lens.lens (\UserPoolType' {autoVerifiedAttributes} -> autoVerifiedAttributes) (\s@UserPoolType' {} a -> s {autoVerifiedAttributes = a} :: UserPoolType) Prelude.. Lens.mapping Lens.coerced

-- | The policies associated with the user pool.
userPoolType_policies :: Lens.Lens' UserPoolType (Prelude.Maybe UserPoolPolicyType)
userPoolType_policies = Lens.lens (\UserPoolType' {policies} -> policies) (\s@UserPoolType' {} a -> s {policies = a} :: UserPoolType)

-- | You can choose to enable case sensitivity on the username input for the
-- selected sign-in option. For example, when this is set to @False@, users
-- will be able to sign in using either \"username\" or \"Username\". This
-- configuration is immutable once it has been set. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
userPoolType_usernameConfiguration :: Lens.Lens' UserPoolType (Prelude.Maybe UsernameConfigurationType)
userPoolType_usernameConfiguration = Lens.lens (\UserPoolType' {usernameConfiguration} -> usernameConfiguration) (\s@UserPoolType' {} a -> s {usernameConfiguration = a} :: UserPoolType)

instance Core.FromJSON UserPoolType where
  parseJSON =
    Core.withObject
      "UserPoolType"
      ( \x ->
          UserPoolType'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "UserPoolTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EmailConfigurationFailure")
            Prelude.<*> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "VerificationMessageTemplate")
            Prelude.<*> (x Core..:? "EstimatedNumberOfUsers")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Domain")
            Prelude.<*> (x Core..:? "CustomDomain")
            Prelude.<*> (x Core..:? "EmailVerificationMessage")
            Prelude.<*> (x Core..:? "SmsAuthenticationMessage")
            Prelude.<*> (x Core..:? "UserPoolAddOns")
            Prelude.<*> (x Core..:? "SchemaAttributes")
            Prelude.<*> (x Core..:? "EmailVerificationSubject")
            Prelude.<*> ( x Core..:? "UsernameAttributes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "AliasAttributes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AccountRecoverySetting")
            Prelude.<*> (x Core..:? "EmailConfiguration")
            Prelude.<*> (x Core..:? "SmsVerificationMessage")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "MfaConfiguration")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "SmsConfigurationFailure")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "LambdaConfig")
            Prelude.<*> (x Core..:? "SmsConfiguration")
            Prelude.<*> (x Core..:? "AdminCreateUserConfig")
            Prelude.<*> (x Core..:? "DeviceConfiguration")
            Prelude.<*> ( x Core..:? "AutoVerifiedAttributes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Policies")
            Prelude.<*> (x Core..:? "UsernameConfiguration")
      )

instance Prelude.Hashable UserPoolType where
  hashWithSalt salt' UserPoolType' {..} =
    salt' `Prelude.hashWithSalt` usernameConfiguration
      `Prelude.hashWithSalt` policies
      `Prelude.hashWithSalt` autoVerifiedAttributes
      `Prelude.hashWithSalt` deviceConfiguration
      `Prelude.hashWithSalt` adminCreateUserConfig
      `Prelude.hashWithSalt` smsConfiguration
      `Prelude.hashWithSalt` lambdaConfig
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` smsConfigurationFailure
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` mfaConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` smsVerificationMessage
      `Prelude.hashWithSalt` emailConfiguration
      `Prelude.hashWithSalt` accountRecoverySetting
      `Prelude.hashWithSalt` aliasAttributes
      `Prelude.hashWithSalt` usernameAttributes
      `Prelude.hashWithSalt` emailVerificationSubject
      `Prelude.hashWithSalt` schemaAttributes
      `Prelude.hashWithSalt` userPoolAddOns
      `Prelude.hashWithSalt` smsAuthenticationMessage
      `Prelude.hashWithSalt` emailVerificationMessage
      `Prelude.hashWithSalt` customDomain
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` estimatedNumberOfUsers
      `Prelude.hashWithSalt` verificationMessageTemplate
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` emailConfigurationFailure
      `Prelude.hashWithSalt` userPoolTags
      `Prelude.hashWithSalt` status

instance Prelude.NFData UserPoolType where
  rnf UserPoolType' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf usernameConfiguration
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf autoVerifiedAttributes
      `Prelude.seq` Prelude.rnf deviceConfiguration
      `Prelude.seq` Prelude.rnf adminCreateUserConfig
      `Prelude.seq` Prelude.rnf smsConfiguration
      `Prelude.seq` Prelude.rnf lambdaConfig
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf smsConfigurationFailure
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf mfaConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf smsVerificationMessage
      `Prelude.seq` Prelude.rnf emailConfiguration
      `Prelude.seq` Prelude.rnf accountRecoverySetting
      `Prelude.seq` Prelude.rnf aliasAttributes
      `Prelude.seq` Prelude.rnf usernameAttributes
      `Prelude.seq` Prelude.rnf emailVerificationSubject
      `Prelude.seq` Prelude.rnf schemaAttributes
      `Prelude.seq` Prelude.rnf userPoolAddOns
      `Prelude.seq` Prelude.rnf smsAuthenticationMessage
      `Prelude.seq` Prelude.rnf emailVerificationMessage
      `Prelude.seq` Prelude.rnf customDomain
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf estimatedNumberOfUsers
      `Prelude.seq` Prelude.rnf verificationMessageTemplate
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf emailConfigurationFailure
      `Prelude.seq` Prelude.rnf userPoolTags
